{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.GCPL
    ( module Bindings.HDF5.PropertyList.OCPL
    
    , GCPL
    , GroupCreationPropertyList(..)
    
    , setLocalHeapSizeHint
    , getLocalHeapSizeHint
    
    , setLinkPhaseChange
    , getLinkPhaseChange
    
    , getEstLinkInfo
    , setEstLinkInfo
    
    , CreationOrder(..)
    , setLinkCreationOrder
    , getLinkCreationOrder
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList.OCPL
import Bindings.HDF5.Raw.H5P
import Foreign.C.Types
import Foreign.Ptr.Conventions

newtype GCPL = GCPL OCPL
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass, ObjectCreationPropertyList)

instance PropertyList GCPL where
    staticPlistClass = Tagged groupCreate

class ObjectCreationPropertyList t => GroupCreationPropertyList t where
instance GroupCreationPropertyList GCPL

setLocalHeapSizeHint :: GroupCreationPropertyList gcpl => gcpl -> CSize -> IO ()
setLocalHeapSizeHint gcpl sz =
    withErrorCheck_ $ 
        h5p_set_local_heap_size_hint (hid gcpl) sz

getLocalHeapSizeHint :: GroupCreationPropertyList gcpl => gcpl -> IO CSize
getLocalHeapSizeHint gcpl =
    withOut_ $ \sz ->
        withErrorCheck_ $
            h5p_get_local_heap_size_hint (hid gcpl) sz

setLinkPhaseChange :: GroupCreationPropertyList gcpl => gcpl -> CUInt -> CUInt -> IO ()
setLinkPhaseChange gcpl maxCompact minDense =
    withErrorCheck_ $
        h5p_set_link_phase_change (hid gcpl) maxCompact minDense

getLinkPhaseChange :: GroupCreationPropertyList gcpl => gcpl -> IO (CUInt, CUInt)
getLinkPhaseChange gcpl =
    withOut $ \maxCompact ->
        withOut_ $ \minDense ->
            withErrorCheck_ $
                h5p_get_link_phase_change (hid gcpl) maxCompact minDense

setEstLinkInfo :: GroupCreationPropertyList gcpl => gcpl -> CUInt -> CUInt -> IO ()
setEstLinkInfo gcpl estNumEntries estNameLen =
    withErrorCheck_ $
        h5p_set_est_link_info (hid gcpl) estNumEntries estNameLen

getEstLinkInfo :: GroupCreationPropertyList gcpl => gcpl -> IO (CUInt, CUInt)
getEstLinkInfo gcpl =
    withOut $ \estNumEntries ->
        withOut_ $ \estNameLen ->
            withErrorCheck_ $
                h5p_get_est_link_info (hid gcpl) estNumEntries estNameLen

data CreationOrder
    = Tracked
    | Indexed
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

creationOrderCode Tracked = h5p_CRT_ORDER_TRACKED
creationOrderCode Indexed = h5p_CRT_ORDER_INDEXED

creationOrder c
    | c == h5p_CRT_ORDER_TRACKED    = Tracked
    | c == h5p_CRT_ORDER_INDEXED    = Indexed
    | otherwise = error ("Unknown CreationOrder code: " ++ show c)

setLinkCreationOrder :: GroupCreationPropertyList gcpl => gcpl -> CreationOrder -> IO ()
setLinkCreationOrder gcpl crtOrderFlags =
    withErrorCheck_ $
        h5p_set_link_creation_order (hid gcpl) (creationOrderCode crtOrderFlags)

getLinkCreationOrder :: GroupCreationPropertyList gcpl => gcpl -> IO CreationOrder
getLinkCreationOrder gcpl =
    fmap creationOrder $
        withOut_ $ \crtOrderFlags ->
            withErrorCheck_ $
             h5p_get_link_creation_order (hid gcpl) crtOrderFlags

