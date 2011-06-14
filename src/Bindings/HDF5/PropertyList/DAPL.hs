{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.DAPL
    ( module Bindings.HDF5.PropertyList.LAPL
    
    , DAPL
    , DatasetAccessPropertyList(..)
    
    , setChunkCache
    , getChunkCache
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList.LAPL
import Bindings.HDF5.Raw.H5P
import Foreign.C.Types
import Foreign.Ptr.Conventions

newtype DAPL = DAPL LAPL
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass, LinkAccessPropertyList)

instance PropertyList DAPL where
    staticPlistClass = Tagged datasetAccess

class LinkAccessPropertyList t => DatasetAccessPropertyList t where
instance DatasetAccessPropertyList DAPL

setChunkCache :: DatasetAccessPropertyList dapl => dapl -> CSize -> CSize -> CDouble -> IO ()
setChunkCache dapl rdccNSlots rdccNBytes rddcW0 =
    withErrorCheck_ $
        h5p_set_chunk_cache (hid dapl) rdccNSlots rdccNBytes rddcW0

getChunkCache :: DatasetAccessPropertyList dapl => dapl -> IO (CSize, CSize, CDouble)
getChunkCache dapl = do
    fmap (\(a,(b,c)) -> (a,b,c)) $
        withOut $ \rdccNSlots ->
            withOut $ \rdccNBytes ->
                withOut_ $ \rddcW0 ->
                    withErrorCheck_ $
                        h5p_get_chunk_cache (hid dapl) rdccNSlots rdccNBytes rddcW0
