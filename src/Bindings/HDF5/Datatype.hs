{-# LANGUAGE TemplateHaskell #-}
module Bindings.HDF5.Datatype
    ( Datatype
    , Class(..)
    , CSet(..)
    , ByteOrder(..)
    , Normalization(..)
    , Pad(..)
    , StringPad(..)
    
    , NativeType(..)
    , nativeTypeOf
    , nativeTypeOf1
    
    , module Bindings.HDF5.Datatype
    ) where

import Bindings.HDF5.Raw.H5
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
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Ptr.Conventions

import Bindings.HDF5.Datatype.Internal

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

ieeeF32be            = Datatype h5t_IEEE_F32BE
ieeeF32le            = Datatype h5t_IEEE_F32LE
ieeeF64be            = Datatype h5t_IEEE_F64BE
ieeeF64le            = Datatype h5t_IEEE_F64LE
stdI8be              = Datatype h5t_STD_I8BE
stdI8le              = Datatype h5t_STD_I8LE
stdI16be             = Datatype h5t_STD_I16BE
stdI16le             = Datatype h5t_STD_I16LE
stdI32be             = Datatype h5t_STD_I32BE
stdI32le             = Datatype h5t_STD_I32LE
stdI64be             = Datatype h5t_STD_I64BE
stdI64le             = Datatype h5t_STD_I64LE
stdU8be              = Datatype h5t_STD_U8BE
stdU8le              = Datatype h5t_STD_U8LE
stdU16be             = Datatype h5t_STD_U16BE
stdU16le             = Datatype h5t_STD_U16LE
stdU32be             = Datatype h5t_STD_U32BE
stdU32le             = Datatype h5t_STD_U32LE
stdU64be             = Datatype h5t_STD_U64BE
stdU64le             = Datatype h5t_STD_U64LE
stdB8be              = Datatype h5t_STD_B8BE
stdB8le              = Datatype h5t_STD_B8LE
stdB16be             = Datatype h5t_STD_B16BE
stdB16le             = Datatype h5t_STD_B16LE
stdB32be             = Datatype h5t_STD_B32BE
stdB32le             = Datatype h5t_STD_B32LE
stdB64be             = Datatype h5t_STD_B64BE
stdB64le             = Datatype h5t_STD_B64LE
stdRefObj            = Datatype h5t_STD_REF_OBJ
stdRefDsetreg        = Datatype h5t_STD_REF_DSETREG
unixD32be            = Datatype h5t_UNIX_D32BE
unixD32le            = Datatype h5t_UNIX_D32LE
unixD64be            = Datatype h5t_UNIX_D64BE
unixD64le            = Datatype h5t_UNIX_D64LE
cS1                  = Datatype h5t_C_S1
fortranS1            = Datatype h5t_FORTRAN_S1
intelI8              = Datatype h5t_INTEL_I8
intelI16             = Datatype h5t_INTEL_I16
intelI32             = Datatype h5t_INTEL_I32
intelI64             = Datatype h5t_INTEL_I64
intelU8              = Datatype h5t_INTEL_U8
intelU16             = Datatype h5t_INTEL_U16
intelU32             = Datatype h5t_INTEL_U32
intelU64             = Datatype h5t_INTEL_U64
intelB8              = Datatype h5t_INTEL_B8
intelB16             = Datatype h5t_INTEL_B16
intelB32             = Datatype h5t_INTEL_B32
intelB64             = Datatype h5t_INTEL_B64
intelF32             = Datatype h5t_INTEL_F32
intelF64             = Datatype h5t_INTEL_F64
alphaI8              = Datatype h5t_ALPHA_I8
alphaI16             = Datatype h5t_ALPHA_I16
alphaI32             = Datatype h5t_ALPHA_I32
alphaI64             = Datatype h5t_ALPHA_I64
alphaU8              = Datatype h5t_ALPHA_U8
alphaU16             = Datatype h5t_ALPHA_U16
alphaU32             = Datatype h5t_ALPHA_U32
alphaU64             = Datatype h5t_ALPHA_U64
alphaB8              = Datatype h5t_ALPHA_B8
alphaB16             = Datatype h5t_ALPHA_B16
alphaB32             = Datatype h5t_ALPHA_B32
alphaB64             = Datatype h5t_ALPHA_B64
alphaF32             = Datatype h5t_ALPHA_F32
alphaF64             = Datatype h5t_ALPHA_F64
mipsI8               = Datatype h5t_MIPS_I8
mipsI16              = Datatype h5t_MIPS_I16
mipsI32              = Datatype h5t_MIPS_I32
mipsI64              = Datatype h5t_MIPS_I64
mipsU8               = Datatype h5t_MIPS_U8
mipsU16              = Datatype h5t_MIPS_U16
mipsU32              = Datatype h5t_MIPS_U32
mipsU64              = Datatype h5t_MIPS_U64
mipsB8               = Datatype h5t_MIPS_B8
mipsB16              = Datatype h5t_MIPS_B16
mipsB32              = Datatype h5t_MIPS_B32
mipsB64              = Datatype h5t_MIPS_B64
mipsF32              = Datatype h5t_MIPS_F32
mipsF64              = Datatype h5t_MIPS_F64
vaxF32               = Datatype h5t_VAX_F32
vaxF64               = Datatype h5t_VAX_F64
nativeChar           = Datatype h5t_NATIVE_CHAR
nativeSchar          = Datatype h5t_NATIVE_SCHAR
nativeUchar          = Datatype h5t_NATIVE_UCHAR
nativeShort          = Datatype h5t_NATIVE_SHORT
nativeUshort         = Datatype h5t_NATIVE_USHORT
nativeInt            = Datatype h5t_NATIVE_INT
nativeUint           = Datatype h5t_NATIVE_UINT
nativeLong           = Datatype h5t_NATIVE_LONG
nativeUlong          = Datatype h5t_NATIVE_ULONG
nativeLlong          = Datatype h5t_NATIVE_LLONG
nativeUllong         = Datatype h5t_NATIVE_ULLONG
nativeFloat          = Datatype h5t_NATIVE_FLOAT
nativeDouble         = Datatype h5t_NATIVE_DOUBLE
nativeLdouble        = Datatype h5t_NATIVE_LDOUBLE
nativeB8             = Datatype h5t_NATIVE_B8
nativeB16            = Datatype h5t_NATIVE_B16
nativeB32            = Datatype h5t_NATIVE_B32
nativeB64            = Datatype h5t_NATIVE_B64
nativeOpaque         = Datatype h5t_NATIVE_OPAQUE
nativeHaddr          = Datatype h5t_NATIVE_HADDR
nativeHsize          = Datatype h5t_NATIVE_HSIZE
nativeHssize         = Datatype h5t_NATIVE_HSSIZE
nativeHerr           = Datatype h5t_NATIVE_HERR
nativeHbool          = Datatype h5t_NATIVE_HBOOL
nativeInt8           = Datatype h5t_NATIVE_INT8
nativeUint8          = Datatype h5t_NATIVE_UINT8
nativeIntLeast8      = Datatype h5t_NATIVE_INT_LEAST8
nativeUintLeast8     = Datatype h5t_NATIVE_UINT_LEAST8
nativeIntFast8       = Datatype h5t_NATIVE_INT_FAST8
nativeUintFast8      = Datatype h5t_NATIVE_UINT_FAST8
nativeInt16          = Datatype h5t_NATIVE_INT16
nativeUint16         = Datatype h5t_NATIVE_UINT16
nativeIntLeast16     = Datatype h5t_NATIVE_INT_LEAST16
nativeUintLeast16    = Datatype h5t_NATIVE_UINT_LEAST16
nativeIntFast16      = Datatype h5t_NATIVE_INT_FAST16
nativeUintFast16     = Datatype h5t_NATIVE_UINT_FAST16
nativeInt32          = Datatype h5t_NATIVE_INT32
nativeUint32         = Datatype h5t_NATIVE_UINT32
nativeIntLeast32     = Datatype h5t_NATIVE_INT_LEAST32
nativeUintLeast32    = Datatype h5t_NATIVE_UINT_LEAST32
nativeIntFast32      = Datatype h5t_NATIVE_INT_FAST32
nativeUintFast32     = Datatype h5t_NATIVE_UINT_FAST32
nativeInt64          = Datatype h5t_NATIVE_INT64
nativeUint64         = Datatype h5t_NATIVE_UINT64
nativeIntLeast64     = Datatype h5t_NATIVE_INT_LEAST64
nativeUintLeast64    = Datatype h5t_NATIVE_UINT_LEAST64
nativeIntFast64      = Datatype h5t_NATIVE_INT_FAST64
nativeUintFast64     = Datatype h5t_NATIVE_UINT_FAST64

if  isIEEE (0 :: Float)
    && floatRadix  (0 :: Float) == 2
    && floatDigits (0 :: Float) == 24
    && floatRange  (0 :: Float) == (-125,128)
    then [d| instance NativeType Float where nativeTypeId = Tagged nativeFloat |]
    else [d| |]

if  isIEEE (0 :: Double)
    && floatRadix  (0 :: Double) == 2
    && floatDigits (0 :: Double) == 53
    && floatRange  (0 :: Double) == (-1021,1024)
    then [d| instance NativeType Double where nativeTypeId = Tagged nativeDouble |]
    else [d| |]

case sizeOf (0 :: Int) of
    1   -> [d| instance NativeType Int where nativeTypeId = Tagged nativeInt8  |]
    2   -> [d| instance NativeType Int where nativeTypeId = Tagged nativeInt16 |]
    4   -> [d| instance NativeType Int where nativeTypeId = Tagged nativeInt32 |]
    8   -> [d| instance NativeType Int where nativeTypeId = Tagged nativeInt64 |]
    _   -> [d| |]

case sizeOf (0 :: Word) of
    1   -> [d| instance NativeType Word where nativeTypeId = Tagged nativeUint8  |]
    2   -> [d| instance NativeType Word where nativeTypeId = Tagged nativeUint16 |]
    4   -> [d| instance NativeType Word where nativeTypeId = Tagged nativeUint32 |]
    8   -> [d| instance NativeType Word where nativeTypeId = Tagged nativeUint64 |]
    _   -> [d| |]

-- * Operations defined on all datatypes

createTypeID :: Class -> CSize -> IO Datatype
createTypeID cls sz =
    fmap Datatype $
        withErrorCheck $
            h5t_create (classCode cls) sz

copyTypeID :: Datatype -> IO Datatype
copyTypeID (Datatype t) =
    fmap Datatype $
        withErrorCheck $
            h5t_copy t

closeTypeID :: Datatype -> IO ()
closeTypeID (Datatype t) =
    withErrorCheck_ $
        h5t_close t

typeIDsEqual :: Datatype -> Datatype -> IO Bool
typeIDsEqual (Datatype t1) (Datatype t2) =
    htriToBool $
        h5t_equal t1 t2

lockTypeID :: Datatype -> IO ()
lockTypeID (Datatype t) =
    withErrorCheck_ $
        h5t_lock t

commitTypeID :: Location t => t -> BS.ByteString -> Datatype -> Maybe LCPL -> Maybe TCPL -> Maybe TAPL -> IO ()
commitTypeID loc name typeId lcpl tcpl tapl =
    withErrorCheck_ $
        BS.useAsCString name $ \name ->
            h5t_commit2 (hid loc) name (hid typeId) (maybe h5p_DEFAULT hid lcpl) (maybe h5p_DEFAULT hid tcpl) (maybe h5p_DEFAULT hid tapl)

openTypeID :: Location t => t -> BS.ByteString -> Maybe TAPL -> IO Datatype
openTypeID loc name tapl =
    fmap Datatype $
        BS.useAsCString name $ \name ->
            withErrorCheck $
                h5t_open2 (hid loc) name (maybe h5p_DEFAULT hid tapl)

commitTypeIDAnonymously :: Location t => t -> Datatype -> Maybe TCPL -> Maybe TAPL -> IO ()
commitTypeIDAnonymously loc typeId tcpl tapl =
    withErrorCheck_ $
        h5t_commit_anon (hid loc) (hid typeId) (maybe h5p_DEFAULT hid tcpl) (maybe h5p_DEFAULT hid tapl)

getTypeCreationPList :: Datatype -> IO TCPL
getTypeCreationPList (Datatype t) =
    fmap uncheckedFromHId $
        withErrorCheck $
            h5t_get_create_plist t

committedTypeID :: Datatype -> IO Bool
committedTypeID (Datatype t) =
    htriToBool $
        h5t_committed t

encodeTypeID :: Datatype -> IO BS.ByteString
encodeTypeID (Datatype t) = 
    withOutByteString $ \buf bufSz ->
        withInOut_ bufSz $ \bufSz ->
            withErrorCheck_ $
                h5t_encode t buf bufSz

decodeTypeID :: BS.ByteString -> IO Datatype
decodeTypeID bs = 
    fmap Datatype $
        BS.unsafeUseAsCString bs $ \buf ->
            h5t_decode (InArray buf)

-- * Operations defined on compound datatypes

insertCompoundTypeMember :: Datatype -> BS.ByteString -> CSize -> Datatype -> IO ()
insertCompoundTypeMember (Datatype parent) name offset (Datatype member) =
    withErrorCheck_ $
        BS.useAsCString name $ \name ->
            h5t_insert parent name offset member

packCompoundType :: Datatype -> IO ()
packCompoundType (Datatype t) = 
    withErrorCheck_ $
        h5t_pack t

-- * Operations defined on enumeration datatypes

-- TODO: figure out good types for these
-- createEnumType :: Datatype -> IO Datatype
-- createEnumType (Datatype base) =
--     fmap Datatype $
--         withErrorCheck $
--             h5t_enum_create base

-- h5t_enum_insert
-- h5t_enum_nameof
-- h5t_enum_valueof

-- * Operations defined on variable-length datatypes

createVLenType :: Datatype -> IO Datatype
createVLenType (Datatype base) =
    fmap Datatype $
        withErrorCheck $
            h5t_vlen_create base

-- * Operations defined on array datatypes

createArrayType :: Datatype -> [HSize] -> IO Datatype
createArrayType (Datatype base) dims =
    fmap Datatype $
        withInList (map hSize dims) $ \dims ->
            withErrorCheck $
                h5t_array_create2 base nDims dims
    where nDims = genericLength dims

getArrayTypeNDims :: Datatype -> IO CInt 
getArrayTypeNDims (Datatype t) =
    withErrorWhen (< 0) $
        h5t_get_array_ndims t

getArrayTypeDims :: Datatype -> IO [HSize]
getArrayTypeDims (Datatype t) = do
    nDims <- getArrayTypeNDims (Datatype t)
    
    fmap (map HSize . fst) $
        withOutList' (fromIntegral nDims) $ \dims ->
            fmap fromIntegral $
                 withErrorWhen (< 0) $
                    h5t_get_array_dims2 t dims
    

-- * Operations defined on opaque datatypes

setOpaqueTypeTag :: Datatype -> BS.ByteString -> IO ()
setOpaqueTypeTag (Datatype t) tag =
    withErrorCheck_ $
        BS.useAsCString tag $ \tag ->
            h5t_set_tag t tag

getOpaqueTypeTag :: Datatype -> IO BS.ByteString
getOpaqueTypeTag (Datatype t) = do
    cstr <- withErrorWhen (nullPtr ==) $
        h5t_get_tag t
    BS.unsafePackMallocCString cstr

-- * Querying property values

getSuperType :: Datatype -> IO Datatype
getSuperType (Datatype t) =
    fmap Datatype $
        withErrorCheck
            $ h5t_get_super t

getTypeClass :: Datatype -> IO Class
getTypeClass (Datatype t) =
    fmap classFromCode $
        withErrorWhen (\(H5T_class_t c) -> c < 0) $
            h5t_get_class t

detectTypeClass :: Datatype -> Class -> IO Bool
detectTypeClass (Datatype t) cls =
    htriToBool $
        h5t_detect_class t (classCode cls)

getTypeSize :: Datatype -> IO CSize
getTypeSize (Datatype t) =
    withErrorWhen (<= 0) $
        h5t_get_size t

getByteOrder :: Datatype -> IO (Maybe ByteOrder)
getByteOrder (Datatype t) =
    fmap byteOrder $
        withErrorCheck $
            h5t_get_order t

getTypePrecision :: Datatype -> IO CSize
getTypePrecision (Datatype t) =
    withErrorWhen (<= 0) $
        h5t_get_precision t

getTypeOffset :: Datatype -> IO CInt
getTypeOffset (Datatype t) =
    withErrorWhen (< 0) $
        h5t_get_offset t

getTypePad :: Datatype -> IO (Pad, Pad)
getTypePad (Datatype t) = do
    (msb, lsb) <- 
        withOut $ \msb ->
            withOut_ $ \lsb -> 
                withErrorCheck $
                    h5t_get_pad t msb lsb
    return (padFromCode msb, padFromCode lsb)

getFloatTypeFields :: Datatype -> IO (CSize, CSize, CSize, CSize, CSize)
getFloatTypeFields (Datatype t) = do
    (spos, (epos, (esize, (mpos, msize)))) <- 
        withOut $ \spos ->
            withOut $ \epos ->
                withOut $ \esize ->
                    withOut $ \mpos -> 
                        withOut_ $ \msize ->
                            withErrorCheck_ $
                                h5t_get_fields t spos epos esize mpos msize
    return (spos, epos, esize, mpos, msize)

getFloatTypeEBias :: Datatype -> IO CSize
getFloatTypeEBias (Datatype t) = do
    withErrorWhen (== 0) $
        h5t_get_ebias t

getFloatTypeNormalization :: Datatype -> IO (Maybe Normalization)
getFloatTypeNormalization (Datatype t) =
    fmap normalization $
        withErrorCheck $
            h5t_get_norm t

getFloatTypeInternalPad :: Datatype -> IO Pad
getFloatTypeInternalPad (Datatype t) = do
    fmap padFromCode $
        withErrorCheck $
            h5t_get_inpad t

getStringPad :: Datatype -> IO StringPad
getStringPad (Datatype t) =
    fmap stringPadFromCode $
        withErrorCheck $
            h5t_get_strpad t

-- type must be enum or compound
getTypeNMembers :: Datatype -> IO CInt
getTypeNMembers (Datatype t) =
    withErrorWhen (< 0) $
        h5t_get_nmembers t

-- type must be enum or compound
getMemberName :: Datatype -> CUInt -> IO BS.ByteString
getMemberName (Datatype t) i = do
    cstr <- withErrorWhen (== nullPtr) $
        h5t_get_member_name t i
    bs <- BS.packCString cstr
    free cstr
    return bs

getMemberIndex :: Datatype -> BS.ByteString -> IO CInt
getMemberIndex (Datatype t) bs =
    withErrorWhen (< 0) $
        BS.useAsCString bs $ \name ->
            h5t_get_member_index t name

-- TODO: implement these
--  (figure out proper error check for this one, the docs are weird)
-- getMemberOffset :: Datatype -> CUInt -> IO CSize
-- getMemberOffset (Datatype t) i =
--     withErrorWhen (< 0)

-- getMemberClass
-- getMemberType
-- getMemberValue
-- getCharSet
-- isVariableString
-- getNativeType 

-- * Setting property values

setTypeSize :: Datatype -> CSize -> IO ()
setTypeSize (Datatype t) sz =
    withErrorCheck_ $
        h5t_set_size t sz

setByteOrder :: Datatype -> Maybe ByteOrder -> IO ()
setByteOrder (Datatype t) order =
    withErrorCheck_ $
        h5t_set_order t (byteOrderCode order)

setTypePrecision :: Datatype -> CSize -> IO ()
setTypePrecision (Datatype t) prec =
    withErrorCheck_ $
        h5t_set_precision t prec

setTypeOffset :: Datatype -> CSize -> IO ()
setTypeOffset (Datatype t) offset =
    withErrorCheck_ $
        h5t_set_offset t offset

setTypePad :: Datatype -> Pad -> Pad -> IO ()
setTypePad (Datatype t) msb lsb =
    withErrorCheck_ $
        h5t_set_pad t (padCode msb) (padCode lsb)

setFloatTypeFields :: Datatype -> CSize -> CSize -> CSize -> CSize -> CSize -> IO ()
setFloatTypeFields (Datatype t) spos epos esize mpos msize =
    withErrorCheck_ $
        h5t_set_fields t spos epos esize mpos msize

setFloatTypeEBias :: Datatype -> CSize -> IO ()
setFloatTypeEBias (Datatype t) ebias =
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