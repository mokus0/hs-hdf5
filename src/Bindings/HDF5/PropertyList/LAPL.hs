{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.LAPL
    ( module Bindings.HDF5.PropertyList
    
    , LAPL
    , LinkAccessPropertyList(..)
    
    , setNLinks
    , getNLinks
    
    , setELinkPrefix
    , getELinkPrefix
    
    , setELinkFAPL
    , getELinkFAPL
    
    , setELinkAccFlags
    , getELinkAccFlags
    ) where

import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList
import Bindings.HDF5.PropertyList.FAPL

import qualified Data.ByteString as BS
import Foreign.Ptr.Conventions
import Foreign.C.Types

class PropertyList t => LinkAccessPropertyList t where
newtype LAPL = LAPL PropertyListID
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass)
instance PropertyList LAPL where
    staticPlistClass = Tagged linkAccess
instance LinkAccessPropertyList LAPL

setNLinks :: LinkAccessPropertyList lapl => lapl -> CSize -> IO ()
setNLinks lapl nLinks =
    withErrorCheck_ $
        h5p_set_nlinks (hid lapl) nLinks

getNLinks :: LinkAccessPropertyList lapl => lapl -> IO CSize
getNLinks lapl =
    withOut_ $ \nLinks ->
        withErrorCheck_ $
            h5p_get_nlinks (hid lapl) nLinks

setELinkPrefix :: LinkAccessPropertyList lapl => lapl -> BS.ByteString -> IO ()
setELinkPrefix lapl prefix =
    withErrorCheck_ $
        BS.useAsCString prefix $ \prefix -> do
            h5p_set_elink_prefix (hid lapl) prefix

getELinkPrefix :: LinkAccessPropertyList lapl => lapl -> IO BS.ByteString
getELinkPrefix lapl = do
    withOutByteString $ \buf bufSz ->
        withErrorWhen (< 0) $
            h5p_get_elink_prefix (hid lapl) buf bufSz

getELinkFAPL :: LinkAccessPropertyList lapl => lapl -> IO FAPL
getELinkFAPL lapl = 
    fmap uncheckedFromHId $
        withErrorCheck $
            h5p_get_elink_fapl (hid lapl)

setELinkFAPL :: LinkAccessPropertyList lapl => lapl -> FAPL -> IO ()
setELinkFAPL lapl fapl = 
    withErrorCheck_ $
        h5p_set_elink_fapl (hid lapl) (hid fapl)

-- TODO: an enumeration type for these flags
setELinkAccFlags :: LinkAccessPropertyList lapl => lapl -> CUInt -> IO ()
setELinkAccFlags lapl flags = 
    withErrorCheck_ $
        h5p_set_elink_acc_flags (hid lapl) flags

getELinkAccFlags :: LinkAccessPropertyList lapl => lapl -> IO CUInt
getELinkAccFlags lapl = 
    withOut_ $ \flags ->
        withErrorCheck_ $
            h5p_get_elink_acc_flags (hid lapl) flags

-- type ELinkTraverse = BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Ptr CUInt -> FAPL -> IO ()
-- h5p_set_elink_cb :: HId_t -> H5L_elink_traverse_t a -> Ptr a -> IO HErr_t
-- h5p_get_elink_cb :: HId_t -> Out (H5L_elink_traverse_t a) -> Out (Ptr a) -> IO HErr_t
