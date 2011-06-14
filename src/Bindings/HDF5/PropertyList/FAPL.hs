{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.FAPL
    ( module Bindings.HDF5.PropertyList
    
    , FAPL
    , FileAccessPropertyList(..)
    
    , setAlignment
    , getAlignment
    
    , setFamilyOffset
    , getFamilyOffset
    ) where

import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList

import Control.Arrow ((***))
import Foreign.Ptr.Conventions

class PropertyList t => FileAccessPropertyList t where
newtype FAPL = FAPL PropertyListID
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass)
instance PropertyList FAPL where
    staticPlistClass = Tagged fileAccess
instance FileAccessPropertyList FAPL

setAlignment :: FileAccessPropertyList fapl => fapl -> HSize -> HSize -> IO ()
setAlignment fapl threshold alignment =
    withErrorCheck_ $
        h5p_set_alignment (hid fapl) (hSize threshold) (hSize alignment)

getAlignment :: FileAccessPropertyList fapl => fapl -> IO (HSize, HSize)
getAlignment fapl =
    fmap (HSize *** HSize) $
        withOut $ \threshold ->
            withOut_ $ \alignment ->
                withErrorCheck_ $
                    h5p_get_alignment (hid fapl) threshold alignment


-- TODO: implement these
-- h5p_set_driver :: HId_t -> HId_t -> Ptr a -> IO HErr_t
-- h5p_get_driver :: HId_t -> IO HId_t
-- h5p_get_driver_info :: HId_t -> IO (Ptr a)

setFamilyOffset :: FileAccessPropertyList fapl => fapl -> HSize -> IO ()
setFamilyOffset fapl offset =
    withErrorCheck_ $
        h5p_set_family_offset (hid fapl) (hSize offset)

getFamilyOffset :: FileAccessPropertyList fapl => fapl -> IO HSize
getFamilyOffset fapl =
    fmap HSize $
        withOut_ $ \offset ->
            withErrorCheck_ $
                h5p_get_family_offset (hid fapl) offset

-- TODO: implement these
-- h5p_set_multi_type :: HId_t -> H5FD_mem_t -> IO HErr_t
-- h5p_get_multi_type :: HId_t -> Out H5FD_mem_t -> IO HErr_t
-- h5p_set_cache :: HId_t -> CInt -> CSize -> CSize -> CDouble -> IO HErr_t
-- h5p_get_cache :: HId_t -> Out CInt -> Out CSize -> Out CSize -> Out CDouble -> IO HErr_t
-- h5p_set_mdc_config :: HId_t -> In H5AC_cache_config_t -> IO HErr_t
-- h5p_get_mdc_config :: HId_t -> Out H5AC_cache_config_t -> IO HErr_t
-- h5p_set_gc_references :: HId_t -> CUInt -> IO HErr_t
-- h5p_get_gc_references :: HId_t -> Out CUInt -> IO HErr_t
-- h5p_set_fclose_degree :: HId_t -> H5F_close_degree_t -> IO HErr_t
-- h5p_get_fclose_degree :: HId_t -> Out H5F_close_degree_t -> IO HErr_t
-- h5p_set_meta_block_size :: HId_t -> HSize_t -> IO HErr_t
-- h5p_get_meta_block_size :: HId_t -> Out HSize_t -> IO HErr_t
-- h5p_set_sieve_buf_size :: HId_t -> CSize -> IO HErr_t
-- h5p_get_sieve_buf_size :: HId_t -> Out CSize -> IO HErr_t
-- h5p_set_small_data_block_size :: HId_t -> HSize_t -> IO HErr_t
-- h5p_get_small_data_block_size :: HId_t -> Out HSize_t -> IO HErr_t
-- h5p_set_libver_bounds :: HId_t -> H5F_libver_t -> H5F_libver_t -> IO HErr_t
-- h5p_get_libver_bounds :: HId_t -> Out H5F_libver_t -> Out H5F_libver_t -> IO HErr_t
-- h5p_set_elink_file_cache_size :: HId_t -> CUInt -> IO HErr_t
-- h5p_get_elink_file_cache_size :: HId_t -> Out CUInt -> IO HErr_t

