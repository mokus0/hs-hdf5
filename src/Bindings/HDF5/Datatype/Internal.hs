{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Datatype.Internal where

import Bindings.HDF5.Core
import Bindings.HDF5.Object
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5T
import Data.Tagged
import Foreign.Storable

newtype Datatype = Datatype HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

instance Object Datatype where
    staticObjectType = Tagged (Just DatatypeObj)

class Storable t => NativeType t where
    nativeTypeId :: Tagged t Datatype

nativeTypeOf :: NativeType t => t -> Datatype
nativeTypeOf it = untagAs it nativeTypeId
    where
        untagAs :: t -> Tagged t a -> a
        untagAs _ = untag

nativeTypeOf1 :: NativeType t => f t -> Datatype
nativeTypeOf1 it = untagAs1 it nativeTypeId
    where
        untagAs1 :: f t -> Tagged t a -> a
        untagAs1 _ = untag

hdfTypeOf :: NativeType t => t -> HId_t
hdfTypeOf = hid . nativeTypeOf

hdfTypeOf1 :: NativeType t => f t -> HId_t
hdfTypeOf1 = hid . nativeTypeOf1


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
