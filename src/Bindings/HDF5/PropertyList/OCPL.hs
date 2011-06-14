{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.OCPL
    ( module Bindings.HDF5.PropertyList
    
    , OCPL
    , ObjectCreationPropertyList(..)
    
    , setAttrPhaseChange
    , getAttrPhaseChange
    
    , setAttrCreationOrder
    , getAttrCreationOrder
    
    , setObjTrackTimes
    , getObjTrackTimes
    
    , modifyFilter
    , setFilter
    
    , getNFilters
    
    , allFiltersAvail
    
    , removeFilter
    
    , setDeflate
    
    , setFletcher32
    ) where

import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Raw.H5Z
import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList

import qualified Data.Vector.Storable as SV
import Foreign.C
import Foreign.Ptr.Conventions

class PropertyList t => ObjectCreationPropertyList t where
newtype OCPL = OCPL PropertyListID
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass)
instance PropertyList OCPL where
    staticPlistClass = Tagged objectCreate
instance ObjectCreationPropertyList OCPL

setAttrPhaseChange :: ObjectCreationPropertyList t => t -> CUInt -> CUInt -> IO ()
setAttrPhaseChange plist max_compact min_dense =
    withErrorCheck_ $
        h5p_set_attr_phase_change (hid plist) max_compact min_dense

getAttrPhaseChange :: ObjectCreationPropertyList t => t -> IO (CUInt, CUInt)
getAttrPhaseChange plist =
    withOut $ \max_compact ->
        withOut_ $ \min_dense ->
            withErrorCheck_ $
                h5p_get_attr_phase_change (hid plist) max_compact min_dense

setAttrCreationOrder :: ObjectCreationPropertyList t => t -> CUInt -> IO ()
setAttrCreationOrder plist crt_order_flags =
    withErrorCheck_ $
        h5p_set_attr_creation_order (hid plist) crt_order_flags

getAttrCreationOrder :: ObjectCreationPropertyList t => t -> IO CUInt
getAttrCreationOrder plist =
    withOut_ $ \crt_order_flags ->
        withErrorCheck_ $
            h5p_get_attr_creation_order (hid plist) crt_order_flags

setObjTrackTimes :: ObjectCreationPropertyList t => t -> Bool -> IO ()
setObjTrackTimes plist track_times =
    withErrorCheck_ $
        h5p_set_obj_track_times (hid plist) (HBool_t $ if track_times then 1 else 0)

getObjTrackTimes :: ObjectCreationPropertyList t => t -> IO Bool
getObjTrackTimes plist =
    fmap (\(HBool_t x) -> x > 0) $
        withOut_ $ \track_times ->
            withErrorCheck_ $
                h5p_get_obj_track_times (hid plist) track_times

modifyFilter :: ObjectCreationPropertyList t => t -> H5Z_filter_t -> Bool -> SV.Vector CUInt -> IO ()
modifyFilter plist filt optional cd_values =
    withErrorCheck_ $
        withInVector cd_values $ \cd_values ->
            h5p_modify_filter (hid plist) filt flags n_elmts cd_values
    where
        flags
            | optional  = h5z_FLAG_OPTIONAL
            | otherwise = 0
        n_elmts = fromIntegral (SV.length cd_values)

setFilter :: ObjectCreationPropertyList t => t -> H5Z_filter_t -> Bool -> SV.Vector CUInt -> IO ()
setFilter plist filt optional cd_values =
    withErrorCheck_ $
        withInVector cd_values $ \cd_values ->
            h5p_set_filter (hid plist) filt flags n_elmts cd_values
    where
        flags
            | optional  = h5z_FLAG_OPTIONAL
            | otherwise = 0
        n_elmts = fromIntegral (SV.length cd_values)

getNFilters :: ObjectCreationPropertyList t => t -> IO CInt
getNFilters plist =
    withErrorWhen (< 0) $
        h5p_get_nfilters (hid plist)

-- getFilter :: ObjectCreationPropertyList t => t -> CUInt -> ...
-- getFilterById

allFiltersAvail :: ObjectCreationPropertyList t => t -> IO Bool
allFiltersAvail plist =
    htriToBool $
        h5p_all_filters_avail (hid plist)

removeFilter :: ObjectCreationPropertyList t => t -> H5Z_filter_t -> IO ()
removeFilter plist filt =
    withErrorCheck_ $
        h5p_remove_filter (hid plist) filt

setDeflate :: ObjectCreationPropertyList t => t -> CUInt -> IO ()
setDeflate plist aggression =
    withErrorCheck_ $
        h5p_set_deflate (hid plist) aggression

setFletcher32 :: ObjectCreationPropertyList t => t -> IO ()
setFletcher32 plist =
    withErrorCheck_ $
        h5p_set_fletcher32 (hid plist)