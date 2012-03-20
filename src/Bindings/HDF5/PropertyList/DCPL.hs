{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.DCPL
    ( module Bindings.HDF5.PropertyList.OCPL
    
    , DCPL
    , DatasetCreationPropertyList(..)
    
    , Layout(..)
    , setLayout
    , getLayout
    
    , setChunk
    , getChunk
    
    , setExternal
    , getExternalCount
    , getExternalN
    , getExternal
    
    , setSZip
    
    , setShuffle
    
    , setNBit
    
    , ScaleType(..)
    , setScaleOffset
    
    , setFillValue
    , getFillValue
    , FillValueDefaultType(..)
    , fillValueDefined
    
    , AllocTime(..)
    , setAllocTime
    , getAllocTime
    
    , FillTime(..)
    , setFillTime
    , getFillTime
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList
import Bindings.HDF5.PropertyList.OCPL
import Bindings.HDF5.Datatype.Internal
import Bindings.HDF5.Raw.H5D
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Raw.H5Z

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.List
import Foreign
import Foreign.C
import Foreign.Ptr.Conventions
import System.Posix.Types

newtype DCPL = DCPL OCPL
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass, ObjectCreationPropertyList)

instance PropertyList DCPL where
    staticPlistClass = Tagged datasetCreate

class ObjectCreationPropertyList t => DatasetCreationPropertyList t where
instance DatasetCreationPropertyList DCPL

data Layout
    = Compact
    | Contiguous
    | Chunked
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

layoutCode :: Layout -> H5D_layout_t
layoutCode Compact      = h5d_COMPACT
layoutCode Contiguous   = h5d_CONTIGUOUS
layoutCode Chunked      = h5d_CHUNKED

layout :: H5D_layout_t -> Layout
layout c
    | c == h5d_COMPACT      = Compact
    | c == h5d_CONTIGUOUS   = Contiguous
    | c == h5d_CHUNKED      = Chunked
    | otherwise = error ("unknown H5D_layout_t: " ++ show c)

setLayout :: DatasetCreationPropertyList t => t -> Layout -> IO ()
setLayout plist layout =
    withErrorCheck_ $
        h5p_set_layout (hid plist) (layoutCode layout)

getLayout :: DatasetCreationPropertyList t => t -> IO Layout
getLayout plist =
    fmap layout $
        withErrorWhen (\(H5D_layout_t c) -> c < 0) $
            h5p_get_layout (hid plist)

setChunk :: DatasetCreationPropertyList t => t -> [HSize] -> IO ()
setChunk plist chunkSizes =
    withErrorCheck_ $
        withInList (map hSize chunkSizes) $ \chunkSizes ->
            h5p_set_chunk (hid plist) n chunkSizes
    where n = genericLength chunkSizes

getChunk :: DatasetCreationPropertyList t => t -> IO [HSize]
getChunk plist = do
    n <- withErrorWhen (< 0) $
        h5p_get_chunk (hid plist) 0 (OutArray nullPtr)
    
    fmap (map HSize) $
        withOutList_ (fromIntegral n) $ \buf ->
            withErrorWhen_ (< 0) $
                h5p_get_chunk (hid plist) n buf

setExternal :: DatasetCreationPropertyList t => t -> BS.ByteString -> COff -> HSize -> IO ()
setExternal plist name offset size =
    withErrorCheck_ $
        BS.useAsCString name $ \name ->
            h5p_set_external (hid plist) name offset (hSize size)

getExternalCount :: DatasetCreationPropertyList t => t -> IO CInt
getExternalCount plist =
    withErrorWhen (< 0) $
        h5p_get_external_count (hid plist)

getExternalN :: DatasetCreationPropertyList t => t -> CUInt -> CSize -> IO (BS.ByteString, COff, HSize)
getExternalN plist idx name_size = do
    let sz = fromIntegral name_size
    name <- mallocBytes sz
    
    (offset, size) <- 
        withOut $ \offset -> 
            withOut_ $ \size ->
                withErrorCheck_ $
                    h5p_get_external (hid plist) idx name_size (OutArray name) offset size
    -- TODO: this will leak memory if an exception is thrown
    
    name <- BS.unsafePackCStringLen (name, sz)
    return (BS.takeWhile (0 /=) name, offset, HSize size)

getExternal :: DatasetCreationPropertyList t => t -> CUInt -> IO (BS.ByteString, COff, HSize)
getExternal plist idx = loop 255
    where
        loop sz = do
            result@(name, _, _) <- getExternalN plist idx sz
            if BS.length name < fromIntegral sz
                then return result
                else do
                    let sz' = 2*sz + 1
                    if sz' > sz
                        then loop sz'
                        else fail "getExternal: name_size overflow, this is almost certainly a programming error in getExternal"

setSZip :: DatasetCreationPropertyList t => t -> CUInt -> CUInt -> IO ()
setSZip plist options_mask pixels_per_block =
    withErrorCheck_ $
        h5p_set_szip (hid plist) options_mask pixels_per_block

setShuffle :: DatasetCreationPropertyList t => t -> IO ()
setShuffle plist =
    withErrorCheck_ $
        h5p_set_shuffle (hid plist)

setNBit :: DatasetCreationPropertyList t => t -> IO ()
setNBit plist =
    withErrorCheck_ $
        h5p_set_nbit (hid plist)

data ScaleType
    = FloatDScale
    | FloatEScale
    | IntScale
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

scaleTypeCode :: ScaleType -> H5Z_SO_scale_type_t
scaleTypeCode FloatDScale = h5z_SO_FLOAT_DSCALE
scaleTypeCode FloatEScale = h5z_SO_FLOAT_ESCALE
scaleTypeCode IntScale    = h5z_SO_INT

setScaleOffset :: DatasetCreationPropertyList t => t -> ScaleType -> CInt -> IO ()
setScaleOffset plist scale_type scale_factor =
    withErrorCheck_ $
        h5p_set_scaleoffset (hid plist) (scaleTypeCode scale_type) scale_factor

setFillValue :: (DatasetCreationPropertyList t, NativeType a) => t -> a -> IO ()
setFillValue plist value =
    withErrorCheck_ $
        withIn value $ \value ->
            h5p_set_fill_value (hid plist) (hdfTypeOf1 value) value

getFillValue :: (DatasetCreationPropertyList t, NativeType a) => t -> IO a
getFillValue plist =
    withOut_ $ \value ->
        withErrorCheck_ $
            h5p_get_fill_value (hid plist) (hdfTypeOf1 value) value

data FillValueDefaultType
    = Undefined
    | DefaultFillValue
    | UserDefined
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

fillValueDefaultType :: H5D_fill_value_t -> FillValueDefaultType
fillValueDefaultType c
    | c == h5d_FILL_VALUE_UNDEFINED     = Undefined
    | c == h5d_FILL_VALUE_DEFAULT       = DefaultFillValue
    | c == h5d_FILL_VALUE_USER_DEFINED  = UserDefined
    | c == h5d_FILL_VALUE_ERROR         = error "fillValueDefined: h5d_FILL_VALUE_ERROR"
    | otherwise = error ("fillValueDefined: unknown H5D_fill_value_t " ++ show c)

fillValueDefined :: DatasetCreationPropertyList t => t -> IO FillValueDefaultType
fillValueDefined plist =
    fmap fillValueDefaultType $
        withOut_ $ \status ->
            withErrorCheck_ $
                h5p_fill_value_defined (hid plist) status

data AllocTime
    = DefaultAllocTime
    | Early
    | Late
    | Incr -- ...emental?
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

allocTimeCode :: AllocTime -> H5D_alloc_time_t
allocTimeCode DefaultAllocTime  = h5d_ALLOC_TIME_DEFAULT
allocTimeCode Early             = h5d_ALLOC_TIME_EARLY
allocTimeCode Late              = h5d_ALLOC_TIME_LATE
allocTimeCode Incr              = h5d_ALLOC_TIME_INCR

allocTime :: H5D_alloc_time_t -> AllocTime
allocTime c
    | c == h5d_ALLOC_TIME_DEFAULT   = DefaultAllocTime
    | c == h5d_ALLOC_TIME_EARLY     = Early
    | c == h5d_ALLOC_TIME_LATE      = Late
    | c == h5d_ALLOC_TIME_INCR      = Incr
    | c == h5d_ALLOC_TIME_ERROR     = error "h5d_ALLOC_TIME_ERROR"
    | otherwise = error ("unknown H5D_alloc_time_t " ++ show c)

setAllocTime :: DatasetCreationPropertyList t => t -> AllocTime -> IO ()
setAllocTime plist alloc_time =
    withErrorCheck_ $
        h5p_set_alloc_time (hid plist) (allocTimeCode alloc_time)

getAllocTime :: DatasetCreationPropertyList t => t -> IO AllocTime
getAllocTime plist =
    fmap allocTime $
        withOut_ $ \alloc_time ->
            withErrorCheck_ $
                h5p_get_alloc_time (hid plist) alloc_time

data FillTime
    = Alloc
    | Never
    | IfSet
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

fillTimeCode :: FillTime -> H5D_fill_time_t
fillTimeCode Alloc = h5d_FILL_TIME_ALLOC
fillTimeCode Never = h5d_FILL_TIME_NEVER
fillTimeCode IfSet = h5d_FILL_TIME_IFSET

fillTime :: H5D_fill_time_t -> FillTime
fillTime c
    | c == h5d_FILL_TIME_ALLOC = Alloc
    | c == h5d_FILL_TIME_NEVER = Never
    | c == h5d_FILL_TIME_IFSET = IfSet

setFillTime :: DatasetCreationPropertyList t => t -> FillTime -> IO ()
setFillTime plist fill_time =
        withErrorCheck_ $
            h5p_set_fill_time (hid plist) (fillTimeCode fill_time)

getFillTime :: DatasetCreationPropertyList t => t -> IO FillTime
getFillTime plist =
    fmap fillTime $
        withOut_ $ \fill_time ->
            withErrorCheck_ $
                h5p_get_fill_time (hid plist) fill_time

