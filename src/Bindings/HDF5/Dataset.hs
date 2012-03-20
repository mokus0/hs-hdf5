{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Dataset
    ( Dataset
    , createDataset
    , createAnonymousDataset
    , openDataset
    , closeDataset
    
    , getDatasetSpace
    , SpaceStatus(..)
    , getDatasetSpaceStatus
    , getDatasetType
    
    , getDatasetCreatePList
    , getDatasetAccessPList
    
    , getDatasetStorageSize
    , getDatasetOffset
    
    , readDataset
    , readDatasetInto
    , writeDataset
    
    , setDatasetExtent
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Dataspace
import Bindings.HDF5.Datatype.Internal
import Bindings.HDF5.Error
import Bindings.HDF5.Object
import Bindings.HDF5.PropertyList.DAPL
import Bindings.HDF5.PropertyList.DCPL
import Bindings.HDF5.PropertyList.DXPL
import Bindings.HDF5.PropertyList.LCPL
import Bindings.HDF5.Raw.H5D
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Raw.H5S
import Control.Monad.ST (RealWorld)
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as SV
import Foreign.Ptr.Conventions

newtype Dataset = Dataset HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

instance Object Dataset where
    staticObjectType = Tagged (Just DatasetObj)

createDataset :: Location loc
    => loc -> BS.ByteString -> Datatype -> Dataspace -> Maybe LCPL -> Maybe DCPL -> Maybe DAPL -> IO Dataset
createDataset loc_id name type_id space_id lcpl_id dcpl_id dapl_id =
    fmap Dataset $
        withErrorCheck $
            BS.useAsCString name $ \name ->
                h5d_create2 (hid loc_id) name (hid type_id) (hid space_id) (maybe h5p_DEFAULT hid lcpl_id) (maybe h5p_DEFAULT hid dcpl_id) (maybe h5p_DEFAULT hid dapl_id)

createAnonymousDataset :: Location loc
    => loc -> Datatype -> Dataspace -> Maybe DCPL -> Maybe DAPL -> IO Dataset
createAnonymousDataset loc_id type_id space_id dcpl_id dapl_id =
    fmap Dataset $
        withErrorCheck $
            h5d_create_anon (hid loc_id) (hid type_id) (hid space_id) (maybe h5p_DEFAULT hid dcpl_id) (maybe h5p_DEFAULT hid dapl_id)

openDataset :: Location loc
    => loc -> BS.ByteString -> Maybe DAPL -> IO Dataset
openDataset loc_id name dapl_id =
    fmap Dataset $
        withErrorCheck $
            BS.useAsCString name $ \name -> 
                h5d_open2 (hid loc_id) name (maybe h5p_DEFAULT hid dapl_id)

closeDataset :: Dataset -> IO ()
closeDataset (Dataset dset_id) =
    withErrorCheck_ (h5d_close dset_id)

getDatasetSpace :: Dataset -> IO Dataspace
getDatasetSpace (Dataset dset_id) = 
    fmap uncheckedFromHId $
        withErrorCheck (h5d_get_space dset_id)

data SpaceStatus
    = NotAllocated
    | Allocated
    | PartAllocated
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

spaceStatusFromCode :: H5D_space_status_t -> SpaceStatus
spaceStatusFromCode c
    | c == h5d_SPACE_STATUS_NOT_ALLOCATED   = NotAllocated
    | c == h5d_SPACE_STATUS_ALLOCATED       = Allocated
    | c == h5d_SPACE_STATUS_PART_ALLOCATED  = PartAllocated
    | otherwise = error ("Unknown H5D_space_status_t " ++ show c)

getDatasetSpaceStatus :: Dataset -> IO SpaceStatus
getDatasetSpaceStatus (Dataset dset_id) =
    fmap spaceStatusFromCode $
        withOut_ $ \status -> 
            withErrorCheck (h5d_get_space_status dset_id status)

getDatasetType :: Dataset -> IO Datatype
getDatasetType (Dataset dset_id) =
    fmap Datatype $
        withErrorCheck $
            h5d_get_type dset_id

getDatasetCreatePList :: Dataset -> IO DCPL
getDatasetCreatePList (Dataset dset_id) =
    fmap uncheckedFromHId $
        withErrorCheck $
            h5d_get_create_plist dset_id

getDatasetAccessPList :: Dataset -> IO DAPL
getDatasetAccessPList (Dataset dset_id) =
    fmap uncheckedFromHId $
        withErrorCheck $
            h5d_get_access_plist dset_id

getDatasetStorageSize :: Dataset -> IO HSize
getDatasetStorageSize (Dataset dset_id) =
    fmap HSize $
        h5d_get_storage_size dset_id

getDatasetOffset :: Dataset -> IO HAddr
getDatasetOffset (Dataset dset_id) =
    fmap HAddr $
        withErrorCheck $
            h5d_get_offset dset_id

readDataset :: NativeType t =>
    Dataset -> Maybe Dataspace -> Maybe DXPL -> IO (SV.Vector t)
readDataset dset@(Dataset dset_id) file_space_id plist_id = do
    effectiveSelection <- maybe (getDatasetSpace dset) return file_space_id
    n <- getSimpleDataspaceExtentNPoints effectiveSelection
    
    withOutVector_ (fromIntegral n) $ \buf ->
        withErrorCheck_ $
            h5d_read dset_id (hdfTypeOf1 buf) h5s_ALL (maybe h5s_ALL hid file_space_id) (maybe h5p_DEFAULT hid plist_id) buf

readDatasetInto :: NativeType t =>
    Dataset -> Maybe Dataspace -> Maybe Dataspace -> Maybe DXPL -> SV.MVector RealWorld t -> IO ()
readDatasetInto dset@(Dataset dset_id) mem_space_id file_space_id plist_id vec = do
    effectiveSelection <- maybe (getDatasetSpace dset) return mem_space_id
    n <- getSimpleDataspaceExtentNPoints effectiveSelection
    
    withOutMVector vec $ \vecSz buf ->
        if fromIntegral n > vecSz
            then fail "readDatasetInto: output vector is too small to contain selection"
            else withErrorCheck_ $
                    h5d_read dset_id (hdfTypeOf1 buf) (maybe h5s_ALL hid mem_space_id) (maybe h5s_ALL hid file_space_id) (maybe h5p_DEFAULT hid plist_id) buf

writeDataset :: NativeType t =>
    Dataset -> Maybe Dataspace -> Maybe Dataspace -> Maybe DXPL -> SV.Vector t -> IO ()
writeDataset (Dataset dset_id) mem_space_id file_space_id plist_id buf =
    withErrorCheck_ $
        withInVector buf $ \buf ->
            h5d_write dset_id (hdfTypeOf1 buf) (maybe h5s_ALL hid mem_space_id) (maybe h5s_ALL hid file_space_id) (maybe h5p_DEFAULT hid plist_id) buf

-- foreign import ccall "wrapper" wrap_iterate_op
--     :: (InOut a -> HId_t -> CUInt -> InArray HSize_t -> InOut b -> IO HErr_t)
--     -> IO (H5D_operator_t a b)
-- 
-- TODO: figure out a good way to properly encapsulate the buffer so that
-- out-of-bounds accesses are impossible.  Probably use a storable vector
-- and check the bounds against the dataspace.
-- Also check whether h5s_ALL is a valid dataspace; Actually I'm not sure how
-- it could be, given that there is no information here about the size
-- of the array apart from whatever is carried in the dataspace.
-- iterateDatasetSelection :: NativeType t => InOutArray t -> Dataspace -> (InOut t -> [HSize] -> IO HErr_t) -> IO ()
-- iterateDatasetSelection buf space_id op = do
--     -- In order to marshall Haskell exceptions through the iterate operations,
--     -- we use 'maxBound' as a "something might have happened" return value
--     -- and pass a description of what that was via these IORefs.
--     -- 
--     -- The assertion mentioned here is that the 'type_id' argument to h5d_iterate
--     -- is passed unchanged to the operator.  If that doesn't hold, then this
--     -- function will need a more complicated type.
--     assertionFailed <- newIORef False
--     exception       <- newIORef Nothing :: IO (IORef (Maybe SomeException))
--     
--     let bufType = hdfTypeOf1 buf
--     
--     op <- wrap_iterate_op $ \elem type_id ndim (InArray point) _operator_data -> do
--         point <- peekArray (fromIntegral ndim) point
--         if type_id == bufType
--             then do
--                 result <- try (op elem (fmap HSize point))
--                 case result of 
--                     Left exc -> do
--                         writeIORef exception (Just exc)
--                         return maxBound
--                     Right x -> return x
--             else do
--                 writeIORef assertionFailed True
--                 return maxBound
--     
--     result <- withErrorCheck_
--         (h5d_iterate buf bufType (hid space_id) op (InOut nullPtr)
--         `finally` freeHaskellFunPtr op)
--     
--     when (result == maxBound) $ do
--         assertionFailed <- readIORef assertionFailed
--         when assertionFailed (throwIO (AssertionFailed "iterateDatasetSelection: operator called with different type_id than h5d_iterate was called with!"))
--         exception <- readIORef exception
--         maybe (return result) throwIO exception
--         
--     return result

setDatasetExtent :: Dataset -> [HSize] -> IO ()
setDatasetExtent (Dataset dset_id) sizes =
    withErrorCheck_ $
        withInList [sz | HSize sz <- sizes] $ \sizes ->
            h5d_set_extent dset_id sizes
