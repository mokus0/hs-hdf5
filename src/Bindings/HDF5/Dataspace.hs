{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Dataspace
    ( Dataspace
    , DataspaceClass(..)
    , SelectionOperator(..)
    , SelectionType(..)
    
    , createDataspace
    , createSimpleDataspace
    , createExpandableDataspace
    
    , copyDataspace
    
    , closeDataspace
    
    , encodeDataspace
    , decodeDataspace
    
    , getSimpleDataspaceExtentNPoints
    , getSimpleDataspaceExtentNDims
    , getSimpleDataspaceExtent
    
    , isSimpleDataspace
    
    , getDataspaceSelectionNPoints
    
    , selectHyperslab
    , selectElements
    
    , getSimpleDataspaceExtentType
    
    , setDataspaceExtentNone
    , copyDataspaceExtent
    , dataspaceExtentsEqual
    
    , selectAll
    , selectNone
    
    , offsetSimpleDataspaceSelection
    
    , selectionValid
    
    , getHyperslabSelection
    , getHyperslabSelectionNBlocks
    , getHyperslabSelectionBlockList
    
    , getElementSelection
    , getElementSelectionNPoints
    , getElementSelectionPointList
    
    , getSelectionBounds
    
    , getSelectionType
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5S
import Control.Exception (assert)
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Foreign
import Foreign.C
import Foreign.Ptr.Conventions

newtype Dataspace = Dataspace HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

data DataspaceClass
    = Scalar
    | Simple
    | Null
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

rawDataspaceClass :: DataspaceClass -> H5S_class_t
rawDataspaceClass Scalar = h5s_SCALAR
rawDataspaceClass Simple = h5s_SIMPLE
rawDataspaceClass Null   = h5s_NULL

dataspaceClass :: H5S_class_t -> DataspaceClass
dataspaceClass c
    | c == h5s_SCALAR   = Scalar
    | c == h5s_SIMPLE   = Simple
    | c == h5s_NULL     = Null
    | otherwise         = error ("unknown dataspace class: " ++ show c)

createDataspace :: DataspaceClass -> IO Dataspace
createDataspace cls = 
    fmap Dataspace $
        withErrorCheck $
            h5s_create (rawDataspaceClass cls)

createSimpleDataspace :: [HSize] -> IO Dataspace
createSimpleDataspace dims = 
    fmap Dataspace $
        withErrorCheck $
            withInList (map hSize dims) $ \dims ->
                h5s_create_simple n dims (InArray nullPtr) 
    where n = genericLength dims

createExpandableDataspace :: [(HSize, Maybe HSize)] -> IO Dataspace
createExpandableDataspace dims =
    fmap Dataspace $
        withErrorCheck $
            withInList (map hSize dimSizes) $ \dimSizes -> 
                withInList (map (maybe h5s_UNLIMITED hSize) dimLimits) $ \dimLimits ->
                    h5s_create_simple n dimSizes dimLimits
    where
        n = genericLength dims
        (dimSizes, dimLimits) = unzip dims

copyDataspace :: Dataspace -> IO Dataspace
copyDataspace (Dataspace space_id) =
    fmap Dataspace $
        withErrorCheck $
            h5s_copy space_id

closeDataspace :: Dataspace -> IO ()
closeDataspace (Dataspace space_id) =
    withErrorCheck_ $
        h5s_close space_id

encodeDataspace :: Dataspace -> IO BS.ByteString
encodeDataspace (Dataspace space_id) = 
    withOutByteString $ \buf bufSz ->
        withInOut_ bufSz $ \bufSz ->
            withErrorCheck_ $
                h5s_encode space_id buf bufSz

decodeDataspace :: BS.ByteString -> IO Dataspace
decodeDataspace bs = BS.unsafeUseAsCString bs $ \buf -> 
    fmap Dataspace $
        withErrorCheck $
            h5s_decode buf

getSimpleDataspaceExtentNPoints :: Dataspace -> IO HSize
getSimpleDataspaceExtentNPoints (Dataspace space_id) =
    fmap fromIntegral $
        withErrorWhen (< 0) $
            h5s_get_simple_extent_npoints space_id

getSimpleDataspaceExtentNDims :: Dataspace -> IO CInt
getSimpleDataspaceExtentNDims (Dataspace space_id) =
    withErrorWhen (< 0) $
        h5s_get_simple_extent_ndims space_id

getSimpleDataspaceExtent :: Dataspace -> IO ([HSize], [Maybe HSize])
getSimpleDataspaceExtent space@(Dataspace space_id) = do
    n <- getSimpleDataspaceExtentNDims space
    
    (dims, (maxDims, n')) <- 
        withOutList (fromIntegral n) $ \dims ->
            withOutList (fromIntegral n) $ \maxDims ->
                withErrorWhen (< 0) $
                    h5s_get_simple_extent_dims space_id dims maxDims
    
    assert (n==n') $ return
        ( (map HSize dims)
        , [ if d == h5s_UNLIMITED then Nothing else Just (HSize d) | d <- maxDims]
        )

isSimpleDataspace :: Dataspace -> IO Bool
isSimpleDataspace (Dataspace space_id) =
    htriToBool $
        h5s_is_simple space_id

getDataspaceSelectionNPoints :: Dataspace -> IO HSize
getDataspaceSelectionNPoints (Dataspace space_id) =
    fmap fromIntegral $
        withErrorWhen (< 0) $
            h5s_get_select_npoints space_id

data SelectionOperator
    = Set
    | Or
    | And
    | Xor
    | NotB
    | NotA
    | Append
    | Prepend
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

rawSelectionOperator :: SelectionOperator -> H5S_seloper_t
rawSelectionOperator Set     = h5s_SELECT_SET
rawSelectionOperator Or      = h5s_SELECT_OR
rawSelectionOperator And     = h5s_SELECT_AND
rawSelectionOperator Xor     = h5s_SELECT_XOR
rawSelectionOperator NotB    = h5s_SELECT_NOTB
rawSelectionOperator NotA    = h5s_SELECT_NOTA
rawSelectionOperator Append  = h5s_SELECT_APPEND
rawSelectionOperator Prepend = h5s_SELECT_PREPEND

selectHyperslab :: Dataspace -> SelectionOperator -> [(HSize, Maybe HSize, HSize, Maybe HSize)] -> IO ()
selectHyperslab space@(Dataspace space_id) oper hyperSlab = do
    nDims <- getSimpleDataspaceExtentNDims space
    
    when (length hyperSlab /= fromIntegral nDims) 
        (fail "selectHyperslab: the given hyperslab has the wrong number of dimensions for this dataspace")
    
    withErrorCheck_ $
        withInList' start $ \start ->
            maybe withNull withInList' stride $ \stride ->
                withInList' count $ \count -> 
                    maybe withNull withInList' block $ \block ->
                        h5s_select_hyperslab space_id (rawSelectionOperator oper) start stride count block
    where
        (start, mbStrides, count, mbBlocks) = unzip4 hyperSlab
        stride
            | any isJust mbStrides  = Just (map (fromMaybe 1) mbStrides)
            | otherwise             = Nothing
        block
            | any isJust mbBlocks  = Just (map (fromMaybe 1) mbBlocks)
            | otherwise             = Nothing
        withNull f = f (InArray nullPtr)
        withInList' = withInList . map hSize

selectElements :: Dataspace -> SelectionOperator -> V.Vector (SV.Vector HSize) -> IO ()
selectElements space@(Dataspace space_id) oper elems = do
    nDims <- getSimpleDataspaceExtentNDims space
    
    let nElems = V.length elems
        packed = SV.generate (nElems * fromIntegral nDims) $ \i -> 
            case i `divMod` nElems of
                (dim, elt) -> elems V.! elt SV.! dim
    
    withErrorCheck_ $
        withInVector packed $ \elems ->
            h5s_select_elements space_id (rawSelectionOperator oper) (fromIntegral nElems) (castWrappedPtr elems)

-- TODO: determine whether to throw an exception or simply report it
-- when the returned value is invalid.
getSimpleDataspaceExtentType :: Dataspace -> IO DataspaceClass
getSimpleDataspaceExtentType (Dataspace space_id) =
    fmap dataspaceClass $
        withErrorWhen (\(H5S_class_t c) -> c < 0) $
            h5s_get_simple_extent_type space_id

setDataspaceExtentNone :: Dataspace -> IO ()
setDataspaceExtentNone (Dataspace space_id) =
    withErrorCheck_ $
        h5s_set_extent_none space_id

copyDataspaceExtent :: Dataspace -> Dataspace -> IO ()
copyDataspaceExtent (Dataspace dst) (Dataspace src) =
    withErrorCheck_ $
        h5s_extent_copy dst src

dataspaceExtentsEqual :: Dataspace -> Dataspace -> IO Bool
dataspaceExtentsEqual (Dataspace s1) (Dataspace s2) =
    htriToBool $
        h5s_extent_equal s1 s2

selectAll :: Dataspace -> IO ()
selectAll (Dataspace space_id) =
    withErrorCheck_ $
        h5s_select_all space_id

selectNone :: Dataspace -> IO ()
selectNone (Dataspace space_id) =
    withErrorCheck_ $
        h5s_select_none space_id

offsetSimpleDataspaceSelection :: Dataspace -> SV.Vector HSSize -> IO ()
offsetSimpleDataspaceSelection space@(Dataspace space_id) offsets = do
    nDims <- getSimpleDataspaceExtentNDims space
    
    when (fromIntegral nDims /= SV.length offsets)
        (fail "offsetSimpleDataspaceSelection: offset vector's length must equal number dimensions in dataspace")
    
    withErrorCheck_ $ 
        withInVector offsets $ \offsets ->
            h5s_offset_simple space_id (castWrappedPtr offsets)

selectionValid :: Dataspace -> IO Bool
selectionValid (Dataspace space_id) =
    htriToBool $
        h5s_select_valid space_id

getHyperslabSelection :: Dataspace -> IO (V.Vector (SV.Vector HSize, SV.Vector HSize))
getHyperslabSelection space = getHyperslabSelectionBlockList space 0 maxBound

getHyperslabSelectionNBlocks :: Dataspace -> IO HSize
getHyperslabSelectionNBlocks (Dataspace space_id) =
    fmap fromIntegral $
        withErrorWhen (< 0) $
            h5s_get_select_hyper_nblocks space_id

getHyperslabSelectionBlockList :: Dataspace -> HSize -> HSize -> IO (V.Vector (SV.Vector HSize, SV.Vector HSize))
getHyperslabSelectionBlockList space@(Dataspace space_id) startBlock numBlocks = do
    n <- getHyperslabSelectionNBlocks space
    nDims <- getSimpleDataspaceExtentNDims space
    
    let nBlks = min numBlocks (n - startBlock)
        blkSize = 2 * fromIntegral nDims
        numSizes = nBlks * blkSize
        
        
    sizes <- withOutVector_ (fromIntegral numSizes) $ \buf ->
        withErrorCheck_ $
            h5s_get_select_hyper_blocklist space_id (hSize startBlock) (hSize nBlks) (castWrappedPtr buf)
    
    let decodePt i = SV.slice i (fromIntegral nDims) sizes
        decodeBlock i = (decodePt b, decodePt (b + fromIntegral nDims))
            where b = i * fromIntegral blkSize
    return $! V.generate (fromIntegral nBlks) decodeBlock

getElementSelection :: Dataspace -> IO (V.Vector (SV.Vector HSize))
getElementSelection space = getElementSelectionPointList space 0 maxBound

getElementSelectionNPoints :: Dataspace -> IO HSize
getElementSelectionNPoints (Dataspace space_id) =
    fmap fromIntegral $
        withErrorWhen (< 0) $
            h5s_get_select_elem_npoints space_id

getElementSelectionPointList :: Dataspace -> HSize -> HSize -> IO (V.Vector (SV.Vector HSize))
getElementSelectionPointList space@(Dataspace space_id) startPoint numPoints = do
    n <- getElementSelectionNPoints space
    nDims <- getSimpleDataspaceExtentNDims space
    
    let nPts = min numPoints (n - startPoint)
        sz = nPts * fromIntegral nDims
    
    rawPoints <- withOutVector_ (fromIntegral sz) $ \buf ->
        withErrorCheck_ $
            h5s_get_select_elem_pointlist space_id (hSize startPoint) (hSize nPts) (castWrappedPtr buf)
    
    let unpackPt i = SV.generate (fromIntegral nDims) $ \j -> rawPoints SV.! (i + j * fromIntegral nPts)
    
    return $! V.generate (fromIntegral nPts) unpackPt

getSelectionBounds :: Dataspace -> IO [(HSize, HSize)]
getSelectionBounds space@(Dataspace space_id) = do
    n <- getSimpleDataspaceExtentNDims space
    
    (start, end) <- 
        withOutList (fromIntegral n) $ \start ->
            withOutList_ (fromIntegral n) $ \end ->
                withErrorCheck_ $
                    h5s_get_select_bounds space_id start end
    
    return [(HSize s, HSize e) | (s,e) <- zip start end]

data SelectionType
    = Points
    | Hyperslabs
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

selectionType :: H5S_sel_type -> Maybe SelectionType
selectionType c
    | c == h5s_SEL_NONE         = Nothing
    | c == h5s_SEL_POINTS       = Just Points
    | c == h5s_SEL_HYPERSLABS   = Just Hyperslabs

getSelectionType :: Dataspace -> IO (Maybe SelectionType)
getSelectionType (Dataspace space_id) = 
    fmap selectionType $
        withErrorWhen (\(H5S_sel_type c) -> c < 0) $
            h5s_get_select_type space_id
