{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Datatype where

import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Raw.H5T
import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList.LCPL
import Bindings.HDF5.PropertyList.TAPL
import Bindings.HDF5.PropertyList.TCPL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Int
import Data.List
import Data.Tagged
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Ptr.Conventions

data Class
    = Integer
    | Float
    | Time
    | String
    | BitField
    | Opaque
    | Compound
    | Reference
    | Enum
    | VLen
    | Array
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

classCode :: Class -> H5T_class_t
classCode Integer   = h5t_INTEGER  
classCode Float     = h5t_FLOAT    
classCode Time      = h5t_TIME     
classCode String    = h5t_STRING   
classCode BitField  = h5t_BITFIELD 
classCode Opaque    = h5t_OPAQUE   
classCode Compound  = h5t_COMPOUND 
classCode Reference = h5t_REFERENCE
classCode Enum      = h5t_ENUM     
classCode VLen      = h5t_VLEN     
classCode Array     = h5t_ARRAY    

classFromCode :: H5T_class_t -> Class
classFromCode c
    | c == h5t_INTEGER      = Integer  
    | c == h5t_FLOAT        = Float    
    | c == h5t_TIME         = Time     
    | c == h5t_STRING       = String   
    | c == h5t_BITFIELD     = BitField 
    | c == h5t_OPAQUE       = Opaque   
    | c == h5t_COMPOUND     = Compound 
    | c == h5t_REFERENCE    = Reference
    | c == h5t_ENUM         = Enum     
    | c == h5t_VLEN         = VLen     
    | c == h5t_ARRAY        = Array    
    | otherwise = error ("Unknown H5T_class_t " ++ show c)

data CSet
    = ASCII
    | Reserved2
    | Reserved3
    | Reserved4
    | Reserved5
    | Reserved6
    | Reserved7
    | Reserved8
    | Reserved9
    | Reserved10
    | Reserved11
    | Reserved12
    | Reserved13
    | Reserved14
    | Reserved15
    | UTF8
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

cSetCode :: CSet -> H5T_cset_t
cSetCode ASCII          = h5t_CSET_ASCII
cSetCode Reserved2      = h5t_CSET_RESERVED_2
cSetCode Reserved3      = h5t_CSET_RESERVED_3
cSetCode Reserved4      = h5t_CSET_RESERVED_4
cSetCode Reserved5      = h5t_CSET_RESERVED_5
cSetCode Reserved6      = h5t_CSET_RESERVED_6
cSetCode Reserved7      = h5t_CSET_RESERVED_7
cSetCode Reserved8      = h5t_CSET_RESERVED_8
cSetCode Reserved9      = h5t_CSET_RESERVED_9
cSetCode Reserved10     = h5t_CSET_RESERVED_10
cSetCode Reserved11     = h5t_CSET_RESERVED_11
cSetCode Reserved12     = h5t_CSET_RESERVED_12
cSetCode Reserved13     = h5t_CSET_RESERVED_13
cSetCode Reserved14     = h5t_CSET_RESERVED_14
cSetCode Reserved15     = h5t_CSET_RESERVED_15
cSetCode UTF8           = h5t_CSET_UTF8

cSetFromCode :: H5T_cset_t -> CSet
cSetFromCode c = case lookup c cSets of
    Just cset -> cset
    Nothing   -> error ("Unknown charset code: " ++ show c)
    where cSets = [ (cSetCode x, x) | x <- [minBound .. maxBound]]

newtype TypeID = TypeID HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

class Storable t => NativeType t where
    nativeTypeId :: Tagged t TypeID

nativeTypeOf :: NativeType t => t -> TypeID
nativeTypeOf it = untagAs it nativeTypeId
    where
        untagAs :: t -> Tagged t a -> a
        untagAs _ = untag

nativeTypeOf1 :: NativeType t => f t -> TypeID
nativeTypeOf1 it = untagAs1 it nativeTypeId
    where
        untagAs1 :: f t -> Tagged t a -> a
        untagAs1 _ = untag

hdfTypeOf :: NativeType t => t -> HId_t
hdfTypeOf = hid . nativeTypeOf

hdfTypeOf1 :: NativeType t => f t -> HId_t
hdfTypeOf1 = hid . nativeTypeOf1

instance NativeType CChar where
    nativeTypeId = Tagged nativeChar

instance NativeType CSChar where
    nativeTypeId = Tagged nativeSchar

instance NativeType CUChar where
    nativeTypeId = Tagged nativeUchar

instance NativeType CShort where
    nativeTypeId = Tagged nativeShort

instance NativeType CUShort where
    nativeTypeId = Tagged nativeUshort


instance NativeType CInt where
    nativeTypeId = Tagged nativeInt

instance NativeType CUInt where
    nativeTypeId = Tagged nativeUint

instance NativeType CLong where
    nativeTypeId = Tagged nativeLong

instance NativeType CULong where
    nativeTypeId = Tagged nativeUlong

instance NativeType CLLong where
    nativeTypeId = Tagged nativeLlong

instance NativeType CULLong where
    nativeTypeId = Tagged nativeUllong

instance NativeType CFloat where
    nativeTypeId = Tagged nativeFloat

instance NativeType CDouble where
    nativeTypeId = Tagged nativeDouble

instance NativeType HAddr where
    nativeTypeId = Tagged nativeHaddr

instance NativeType HSize where
    nativeTypeId = Tagged nativeHsize

instance NativeType HSSize where
    nativeTypeId = Tagged nativeHssize

instance NativeType HErr_t where
    nativeTypeId = Tagged nativeHerr

instance NativeType HBool_t where
    nativeTypeId = Tagged nativeHbool


instance NativeType Int8 where
    nativeTypeId = Tagged nativeInt8

instance NativeType Int16 where
    nativeTypeId = Tagged nativeInt16

instance NativeType Int32 where
    nativeTypeId = Tagged nativeInt32

instance NativeType Int64 where
    nativeTypeId = Tagged nativeInt64

instance NativeType Word8 where
    nativeTypeId = Tagged nativeUint8

instance NativeType Word16 where
    nativeTypeId = Tagged nativeUint16

instance NativeType Word32 where
    nativeTypeId = Tagged nativeUint32

instance NativeType Word64 where
    nativeTypeId = Tagged nativeUint64

ieeeF32be            = TypeID h5t_IEEE_F32BE
ieeeF32le            = TypeID h5t_IEEE_F32LE
ieeeF64be            = TypeID h5t_IEEE_F64BE
ieeeF64le            = TypeID h5t_IEEE_F64LE
stdI8be              = TypeID h5t_STD_I8BE
stdI8le              = TypeID h5t_STD_I8LE
stdI16be             = TypeID h5t_STD_I16BE
stdI16le             = TypeID h5t_STD_I16LE
stdI32be             = TypeID h5t_STD_I32BE
stdI32le             = TypeID h5t_STD_I32LE
stdI64be             = TypeID h5t_STD_I64BE
stdI64le             = TypeID h5t_STD_I64LE
stdU8be              = TypeID h5t_STD_U8BE
stdU8le              = TypeID h5t_STD_U8LE
stdU16be             = TypeID h5t_STD_U16BE
stdU16le             = TypeID h5t_STD_U16LE
stdU32be             = TypeID h5t_STD_U32BE
stdU32le             = TypeID h5t_STD_U32LE
stdU64be             = TypeID h5t_STD_U64BE
stdU64le             = TypeID h5t_STD_U64LE
stdB8be              = TypeID h5t_STD_B8BE
stdB8le              = TypeID h5t_STD_B8LE
stdB16be             = TypeID h5t_STD_B16BE
stdB16le             = TypeID h5t_STD_B16LE
stdB32be             = TypeID h5t_STD_B32BE
stdB32le             = TypeID h5t_STD_B32LE
stdB64be             = TypeID h5t_STD_B64BE
stdB64le             = TypeID h5t_STD_B64LE
stdRefObj            = TypeID h5t_STD_REF_OBJ
stdRefDsetreg        = TypeID h5t_STD_REF_DSETREG
unixD32be            = TypeID h5t_UNIX_D32BE
unixD32le            = TypeID h5t_UNIX_D32LE
unixD64be            = TypeID h5t_UNIX_D64BE
unixD64le            = TypeID h5t_UNIX_D64LE
cS1                  = TypeID h5t_C_S1
fortranS1            = TypeID h5t_FORTRAN_S1
intelI8              = TypeID h5t_INTEL_I8
intelI16             = TypeID h5t_INTEL_I16
intelI32             = TypeID h5t_INTEL_I32
intelI64             = TypeID h5t_INTEL_I64
intelU8              = TypeID h5t_INTEL_U8
intelU16             = TypeID h5t_INTEL_U16
intelU32             = TypeID h5t_INTEL_U32
intelU64             = TypeID h5t_INTEL_U64
intelB8              = TypeID h5t_INTEL_B8
intelB16             = TypeID h5t_INTEL_B16
intelB32             = TypeID h5t_INTEL_B32
intelB64             = TypeID h5t_INTEL_B64
intelF32             = TypeID h5t_INTEL_F32
intelF64             = TypeID h5t_INTEL_F64
alphaI8              = TypeID h5t_ALPHA_I8
alphaI16             = TypeID h5t_ALPHA_I16
alphaI32             = TypeID h5t_ALPHA_I32
alphaI64             = TypeID h5t_ALPHA_I64
alphaU8              = TypeID h5t_ALPHA_U8
alphaU16             = TypeID h5t_ALPHA_U16
alphaU32             = TypeID h5t_ALPHA_U32
alphaU64             = TypeID h5t_ALPHA_U64
alphaB8              = TypeID h5t_ALPHA_B8
alphaB16             = TypeID h5t_ALPHA_B16
alphaB32             = TypeID h5t_ALPHA_B32
alphaB64             = TypeID h5t_ALPHA_B64
alphaF32             = TypeID h5t_ALPHA_F32
alphaF64             = TypeID h5t_ALPHA_F64
mipsI8               = TypeID h5t_MIPS_I8
mipsI16              = TypeID h5t_MIPS_I16
mipsI32              = TypeID h5t_MIPS_I32
mipsI64              = TypeID h5t_MIPS_I64
mipsU8               = TypeID h5t_MIPS_U8
mipsU16              = TypeID h5t_MIPS_U16
mipsU32              = TypeID h5t_MIPS_U32
mipsU64              = TypeID h5t_MIPS_U64
mipsB8               = TypeID h5t_MIPS_B8
mipsB16              = TypeID h5t_MIPS_B16
mipsB32              = TypeID h5t_MIPS_B32
mipsB64              = TypeID h5t_MIPS_B64
mipsF32              = TypeID h5t_MIPS_F32
mipsF64              = TypeID h5t_MIPS_F64
vaxF32               = TypeID h5t_VAX_F32
vaxF64               = TypeID h5t_VAX_F64
nativeChar           = TypeID h5t_NATIVE_CHAR
nativeSchar          = TypeID h5t_NATIVE_SCHAR
nativeUchar          = TypeID h5t_NATIVE_UCHAR
nativeShort          = TypeID h5t_NATIVE_SHORT
nativeUshort         = TypeID h5t_NATIVE_USHORT
nativeInt            = TypeID h5t_NATIVE_INT
nativeUint           = TypeID h5t_NATIVE_UINT
nativeLong           = TypeID h5t_NATIVE_LONG
nativeUlong          = TypeID h5t_NATIVE_ULONG
nativeLlong          = TypeID h5t_NATIVE_LLONG
nativeUllong         = TypeID h5t_NATIVE_ULLONG
nativeFloat          = TypeID h5t_NATIVE_FLOAT
nativeDouble         = TypeID h5t_NATIVE_DOUBLE
nativeLdouble        = TypeID h5t_NATIVE_LDOUBLE
nativeB8             = TypeID h5t_NATIVE_B8
nativeB16            = TypeID h5t_NATIVE_B16
nativeB32            = TypeID h5t_NATIVE_B32
nativeB64            = TypeID h5t_NATIVE_B64
nativeOpaque         = TypeID h5t_NATIVE_OPAQUE
nativeHaddr          = TypeID h5t_NATIVE_HADDR
nativeHsize          = TypeID h5t_NATIVE_HSIZE
nativeHssize         = TypeID h5t_NATIVE_HSSIZE
nativeHerr           = TypeID h5t_NATIVE_HERR
nativeHbool          = TypeID h5t_NATIVE_HBOOL
nativeInt8           = TypeID h5t_NATIVE_INT8
nativeUint8          = TypeID h5t_NATIVE_UINT8
nativeIntLeast8      = TypeID h5t_NATIVE_INT_LEAST8
nativeUintLeast8     = TypeID h5t_NATIVE_UINT_LEAST8
nativeIntFast8       = TypeID h5t_NATIVE_INT_FAST8
nativeUintFast8      = TypeID h5t_NATIVE_UINT_FAST8
nativeInt16          = TypeID h5t_NATIVE_INT16
nativeUint16         = TypeID h5t_NATIVE_UINT16
nativeIntLeast16     = TypeID h5t_NATIVE_INT_LEAST16
nativeUintLeast16    = TypeID h5t_NATIVE_UINT_LEAST16
nativeIntFast16      = TypeID h5t_NATIVE_INT_FAST16
nativeUintFast16     = TypeID h5t_NATIVE_UINT_FAST16
nativeInt32          = TypeID h5t_NATIVE_INT32
nativeUint32         = TypeID h5t_NATIVE_UINT32
nativeIntLeast32     = TypeID h5t_NATIVE_INT_LEAST32
nativeUintLeast32    = TypeID h5t_NATIVE_UINT_LEAST32
nativeIntFast32      = TypeID h5t_NATIVE_INT_FAST32
nativeUintFast32     = TypeID h5t_NATIVE_UINT_FAST32
nativeInt64          = TypeID h5t_NATIVE_INT64
nativeUint64         = TypeID h5t_NATIVE_UINT64
nativeIntLeast64     = TypeID h5t_NATIVE_INT_LEAST64
nativeUintLeast64    = TypeID h5t_NATIVE_UINT_LEAST64
nativeIntFast64      = TypeID h5t_NATIVE_INT_FAST64
nativeUintFast64     = TypeID h5t_NATIVE_UINT_FAST64

-- * Operations defined on all datatypes

createTypeID :: Class -> CSize -> IO TypeID
createTypeID cls sz =
    fmap TypeID $
        withErrorCheck $
            h5t_create (classCode cls) sz

copyTypeID :: TypeID -> IO TypeID
copyTypeID (TypeID t) =
    fmap TypeID $
        withErrorCheck $
            h5t_copy t

closeTypeID :: TypeID -> IO ()
closeTypeID (TypeID t) =
    withErrorCheck_ $
        h5t_close t

typeIDsEqual :: TypeID -> TypeID -> IO Bool
typeIDsEqual (TypeID t1) (TypeID t2) =
    htriToBool $
        h5t_equal t1 t2

lockTypeID :: TypeID -> IO ()
lockTypeID (TypeID t) =
    withErrorCheck_ $
        h5t_lock t

commitTypeID :: Location t => t -> BS.ByteString -> TypeID -> Maybe LCPL -> Maybe TCPL -> Maybe TAPL -> IO ()
commitTypeID loc name typeId lcpl tcpl tapl =
    withErrorCheck_ $
        BS.useAsCString name $ \name ->
            h5t_commit2 (hid loc) name (hid typeId) (maybe h5p_DEFAULT hid lcpl) (maybe h5p_DEFAULT hid tcpl) (maybe h5p_DEFAULT hid tapl)

openTypeID :: Location t => t -> BS.ByteString -> Maybe TAPL -> IO TypeID
openTypeID loc name tapl =
    fmap TypeID $
        BS.useAsCString name $ \name ->
            withErrorCheck $
                h5t_open2 (hid loc) name (maybe h5p_DEFAULT hid tapl)

commitTypeIDAnonymously :: Location t => t -> TypeID -> Maybe TCPL -> Maybe TAPL -> IO ()
commitTypeIDAnonymously loc typeId tcpl tapl =
    withErrorCheck_ $
        h5t_commit_anon (hid loc) (hid typeId) (maybe h5p_DEFAULT hid tcpl) (maybe h5p_DEFAULT hid tapl)

getTypeCreationPList :: TypeID -> IO TCPL
getTypeCreationPList (TypeID t) =
    fmap uncheckedFromHId $
        withErrorCheck $
            h5t_get_create_plist t

committedTypeID :: TypeID -> IO Bool
committedTypeID (TypeID t) =
    htriToBool $
        h5t_committed t

encodeTypeID :: TypeID -> IO BS.ByteString
encodeTypeID (TypeID t) = 
    withOutByteString $ \buf bufSz ->
        withInOut_ bufSz $ \bufSz ->
            withErrorCheck_ $
                h5t_encode t buf bufSz

decodeTypeID :: BS.ByteString -> IO TypeID
decodeTypeID bs = 
    fmap TypeID $
        BS.unsafeUseAsCString bs $ \buf ->
            h5t_decode (InArray buf)

-- * Operations defined on compound datatypes

insertCompoundTypeMember :: TypeID -> BS.ByteString -> CSize -> TypeID -> IO ()
insertCompoundTypeMember (TypeID parent) name offset (TypeID member) =
    withErrorCheck_ $
        BS.useAsCString name $ \name ->
            h5t_insert parent name offset member

packCompoundType :: TypeID -> IO ()
packCompoundType (TypeID t) = 
    withErrorCheck_ $
        h5t_pack t

-- * Operations defined on enumeration datatypes

-- TODO: figure out good types for these
-- createEnumType :: TypeID -> IO TypeID
-- createEnumType (TypeID base) =
--     fmap TypeID $
--         withErrorCheck $
--             h5t_enum_create base

-- h5t_enum_insert
-- h5t_enum_nameof
-- h5t_enum_valueof

-- * Operations defined on variable-length datatypes

createVLenType :: TypeID -> IO TypeID
createVLenType (TypeID base) =
    fmap TypeID $
        withErrorCheck $
            h5t_vlen_create base

-- * Operations defined on array datatypes

createArrayType :: TypeID -> [HSize] -> IO TypeID
createArrayType (TypeID base) dims =
    fmap TypeID $
        withInList (map hSize dims) $ \dims ->
            withErrorCheck $
                h5t_array_create2 base nDims dims
    where nDims = genericLength dims

getArrayTypeNDims :: TypeID -> IO CInt 
getArrayTypeNDims (TypeID t) =
    withErrorWhen (< 0) $
        h5t_get_array_ndims t

getArrayTypeDims :: TypeID -> IO [HSize]
getArrayTypeDims (TypeID t) = do
    nDims <- getArrayTypeNDims (TypeID t)
    
    fmap (map HSize . fst) $
        withOutList' (fromIntegral nDims) $ \dims ->
            fmap fromIntegral $
                 withErrorWhen (< 0) $
                    h5t_get_array_dims2 t dims
    

-- * Operations defined on opaque datatypes

setOpaqueTypeTag :: TypeID -> BS.ByteString -> IO ()
setOpaqueTypeTag (TypeID t) tag =
    withErrorCheck_ $
        BS.useAsCString tag $ \tag ->
            h5t_set_tag t tag

getOpaqueTypeTag :: TypeID -> IO BS.ByteString
getOpaqueTypeTag (TypeID t) = do
    cstr <- withErrorWhen (nullPtr ==) $
        h5t_get_tag t
    BS.unsafePackMallocCString cstr

-- * Querying property values

getSuperType :: TypeID -> IO TypeID
getSuperType (TypeID t) =
    fmap TypeID $
        withErrorCheck
            $ h5t_get_super t

getTypeClass :: TypeID -> IO Class
getTypeClass (TypeID t) =
    fmap classFromCode $
        withErrorWhen (\(H5T_class_t c) -> c < 0) $
            h5t_get_class t

detectTypeClass :: TypeID -> Class -> IO Bool
detectTypeClass (TypeID t) cls =
    htriToBool $
        h5t_detect_class t (classCode cls)

getTypeSize :: TypeID -> IO CSize
getTypeSize (TypeID t) =
    withErrorWhen (<= 0) $
        h5t_get_size t

data ByteOrder
    = LE
    | BE
    | VAX
    | Mixed
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

byteOrderCode (Just LE)     = h5t_ORDER_LE
byteOrderCode (Just BE)     = h5t_ORDER_BE
byteOrderCode (Just VAX)    = h5t_ORDER_VAX
byteOrderCode (Just Mixed)  = h5t_ORDER_MIXED
byteOrderCode Nothing       = h5t_ORDER_NONE

byteOrder c
    | c == h5t_ORDER_LE     = Just LE
    | c == h5t_ORDER_BE     = Just BE
    | c == h5t_ORDER_VAX    = Just VAX
    | c == h5t_ORDER_MIXED  = Just Mixed
    | c == h5t_ORDER_NONE   = Nothing

getByteOrder :: TypeID -> IO (Maybe ByteOrder)
getByteOrder (TypeID t) =
    fmap byteOrder $
        withErrorCheck $
            h5t_get_order t

getTypePrecision :: TypeID -> IO CSize
getTypePrecision (TypeID t) =
    withErrorWhen (<= 0) $
        h5t_get_precision t

getTypeOffset :: TypeID -> IO CInt
getTypeOffset (TypeID t) =
    withErrorWhen (< 0) $
        h5t_get_offset t

data Pad
    = Zero
    | One
    | Background
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

padCode     :: Pad -> H5T_pad_t
padCode Zero        = h5t_PAD_ZERO
padCode One         = h5t_PAD_ONE
padCode Background  = h5t_PAD_BACKGROUND

padFromCode :: H5T_pad_t -> Pad
padFromCode c
    | c == h5t_PAD_ZERO         = Zero
    | c == h5t_PAD_ONE          = One
    | c == h5t_PAD_BACKGROUND   = Background
    | otherwise = error ("Unknown Pad code " ++ show c)

getTypePad :: TypeID -> IO (Pad, Pad)
getTypePad (TypeID t) = do
    (msb, lsb) <- 
        withOut $ \msb ->
            withOut_ $ \lsb -> 
                withErrorCheck $
                    h5t_get_pad t msb lsb
    return (padFromCode msb, padFromCode lsb)

getFloatTypeFields :: TypeID -> IO (CSize, CSize, CSize, CSize, CSize)
getFloatTypeFields (TypeID t) = do
    (spos, (epos, (esize, (mpos, msize)))) <- 
        withOut $ \spos ->
            withOut $ \epos ->
                withOut $ \esize ->
                    withOut $ \mpos -> 
                        withOut_ $ \msize ->
                            withErrorCheck_ $
                                h5t_get_fields t spos epos esize mpos msize
    return (spos, epos, esize, mpos, msize)

getFloatTypeEBias :: TypeID -> IO CSize
getFloatTypeEBias (TypeID t) = do
    withErrorWhen (== 0) $
        h5t_get_ebias t

data Normalization
    = Implied
    | MSBSet
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance HDFResultType H5T_norm_t where
    isError (H5T_norm_t c) = c < 0

normalization c
    | c == h5t_NORM_IMPLIED = Just Implied
    | c == h5t_NORM_MSBSET  = Just MSBSet
    | c == h5t_NORM_NONE    = Nothing
    | otherwise = error "Unknown H5T_norm_t value"

getFloatTypeNormalization :: TypeID -> IO (Maybe Normalization)
getFloatTypeNormalization (TypeID t) =
    fmap normalization $
        withErrorCheck $
            h5t_get_norm t

getFloatTypeInternalPad :: TypeID -> IO Pad
getFloatTypeInternalPad (TypeID t) = do
    fmap padFromCode $
        withErrorCheck $
            h5t_get_inpad t

data StringPad
    = NullTerm
    | NullPad
    | SpacePad
    | StringPad_Reserved3
    | StringPad_Reserved4
    | StringPad_Reserved5
    | StringPad_Reserved6
    | StringPad_Reserved7
    | StringPad_Reserved8
    | StringPad_Reserved9
    | StringPad_Reserved10
    | StringPad_Reserved11
    | StringPad_Reserved12
    | StringPad_Reserved13
    | StringPad_Reserved14
    | StringPad_Reserved15
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

stringPadCode NullTerm              = h5t_STR_NULLTERM
stringPadCode NullPad               = h5t_STR_NULLPAD
stringPadCode SpacePad              = h5t_STR_SPACEPAD
stringPadCode StringPad_Reserved3   = h5t_STR_RESERVED_3
stringPadCode StringPad_Reserved4   = h5t_STR_RESERVED_4
stringPadCode StringPad_Reserved5   = h5t_STR_RESERVED_5
stringPadCode StringPad_Reserved6   = h5t_STR_RESERVED_6
stringPadCode StringPad_Reserved7   = h5t_STR_RESERVED_7
stringPadCode StringPad_Reserved8   = h5t_STR_RESERVED_8
stringPadCode StringPad_Reserved9   = h5t_STR_RESERVED_9
stringPadCode StringPad_Reserved10  = h5t_STR_RESERVED_10
stringPadCode StringPad_Reserved11  = h5t_STR_RESERVED_11
stringPadCode StringPad_Reserved12  = h5t_STR_RESERVED_12
stringPadCode StringPad_Reserved13  = h5t_STR_RESERVED_13
stringPadCode StringPad_Reserved14  = h5t_STR_RESERVED_14
stringPadCode StringPad_Reserved15  = h5t_STR_RESERVED_15

stringPadFromCode c
    | c == h5t_STR_NULLTERM     = NullTerm
    | c == h5t_STR_NULLPAD      = NullPad
    | c == h5t_STR_SPACEPAD     = SpacePad
    | c == h5t_STR_RESERVED_3   = StringPad_Reserved3
    | c == h5t_STR_RESERVED_4   = StringPad_Reserved4
    | c == h5t_STR_RESERVED_5   = StringPad_Reserved5
    | c == h5t_STR_RESERVED_6   = StringPad_Reserved6
    | c == h5t_STR_RESERVED_7   = StringPad_Reserved7
    | c == h5t_STR_RESERVED_8   = StringPad_Reserved8
    | c == h5t_STR_RESERVED_9   = StringPad_Reserved9
    | c == h5t_STR_RESERVED_10  = StringPad_Reserved10
    | c == h5t_STR_RESERVED_11  = StringPad_Reserved11
    | c == h5t_STR_RESERVED_12  = StringPad_Reserved12
    | c == h5t_STR_RESERVED_13  = StringPad_Reserved13
    | c == h5t_STR_RESERVED_14  = StringPad_Reserved14
    | c == h5t_STR_RESERVED_15  = StringPad_Reserved15
    | otherwise = error ("Unknown StringPad code " ++ show c)

getStringPad :: TypeID -> IO StringPad
getStringPad (TypeID t) =
    fmap stringPadFromCode $
        withErrorCheck $
            h5t_get_strpad t

-- type must be enum or compound
getTypeNMembers :: TypeID -> IO CInt
getTypeNMembers (TypeID t) =
    withErrorWhen (< 0) $
        h5t_get_nmembers t

-- type must be enum or compound
getMemberName :: TypeID -> CUInt -> IO BS.ByteString
getMemberName (TypeID t) i = do
    cstr <- withErrorWhen (== nullPtr) $
        h5t_get_member_name t i
    bs <- BS.packCString cstr
    free cstr
    return bs

getMemberIndex :: TypeID -> BS.ByteString -> IO CInt
getMemberIndex (TypeID t) bs =
    withErrorWhen (< 0) $
        BS.useAsCString bs $ \name ->
            h5t_get_member_index t name

-- TODO: implement these
--  (figure out proper error check for this one, the docs are weird)
-- getMemberOffset :: TypeID -> CUInt -> IO CSize
-- getMemberOffset (TypeID t) i =
--     withErrorWhen (< 0)

-- getMemberClass
-- getMemberType
-- getMemberValue
-- getCharSet
-- isVariableString
-- getNativeType 

-- * Setting property values

setTypeSize :: TypeID -> CSize -> IO ()
setTypeSize (TypeID t) sz =
    withErrorCheck_ $
        h5t_set_size t sz

setByteOrder :: TypeID -> Maybe ByteOrder -> IO ()
setByteOrder (TypeID t) order =
    withErrorCheck_ $
        h5t_set_order t (byteOrderCode order)

setTypePrecision :: TypeID -> CSize -> IO ()
setTypePrecision (TypeID t) prec =
    withErrorCheck_ $
        h5t_set_precision t prec

setTypeOffset :: TypeID -> CSize -> IO ()
setTypeOffset (TypeID t) offset =
    withErrorCheck_ $
        h5t_set_offset t offset

setTypePad :: TypeID -> Pad -> Pad -> IO ()
setTypePad (TypeID t) msb lsb =
    withErrorCheck_ $
        h5t_set_pad t (padCode msb) (padCode lsb)

setFloatTypeFields :: TypeID -> CSize -> CSize -> CSize -> CSize -> CSize -> IO ()
setFloatTypeFields (TypeID t) spos epos esize mpos msize =
    withErrorCheck_ $
        h5t_set_fields t spos epos esize mpos msize

setFloatTypeEBias :: TypeID -> CSize -> IO ()
setFloatTypeEBias (TypeID t) ebias =
    withErrorCheck_ $
        h5t_set_ebias t ebias

-- TODO: implement these
-- setFloatTypeNormalization ::
-- setFloatTypeInternalPad ::
-- setStringPad

-- * Type conversion database

-- registerTypeConversion
-- unregisterTypeConversion
-- findTypeConversion
-- isCompilerConversion
-- convertData