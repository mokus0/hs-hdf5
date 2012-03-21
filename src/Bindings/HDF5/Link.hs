{-# LANGUAGE ForeignFunctionInterface #-}
module Bindings.HDF5.Link
    ( createHardLink
    , createSoftLink
    , createExternalLink
    
    , doesLinkExist
    
    , moveLink
    , copyLink
    , deleteLink
    
    , LinkType(..)
    , LinkInfo(..)
    , getLinkInfo
    
    , getSymLinkVal
    
    , iterateLinks
    , iterateLinksByName
    
    , visitLinks
    , visitLinksByName
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Datatype.Internal
import Bindings.HDF5.Error
import Bindings.HDF5.Group
import Bindings.HDF5.PropertyList.LAPL
import Bindings.HDF5.PropertyList.LCPL
import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5L
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Raw.Util
import Control.Exception (SomeException, try, finally, throwIO)
import qualified Data.ByteString as BS
import Data.IORef
import Foreign
import Foreign.C
import Foreign.Ptr.Conventions

createHardLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createHardLink src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \srcName ->
            BS.useAsCString dstName $ \dstName ->
                h5l_create_hard (hid src) srcName (hid dst) dstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

createSoftLink :: Location dst => BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createSoftLink srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \srcName ->
            BS.useAsCString dstName $ \dstName ->
                h5l_create_soft srcName (hid dst) dstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

createExternalLink :: Location loc => BS.ByteString -> BS.ByteString -> loc -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
createExternalLink file obj loc name lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString file $ \file ->
            BS.useAsCString obj $ \obj ->
                BS.useAsCString name $ \name ->
                    h5l_create_external file obj (hid loc) name (maybe h5p_DEFAULT hid lcpl) (maybe h5p_DEFAULT hid lapl)

doesLinkExist :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO Bool
doesLinkExist loc name lapl =
    htriToBool $
        BS.useAsCString name $ \name ->
            h5l_exists (hid loc) name (maybe h5p_DEFAULT hid lapl)

moveLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
moveLink  src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \srcName ->
            BS.useAsCString dstName $ \dstName ->
                h5l_move (hid src) srcName (hid dst) dstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

copyLink :: (Location src, Location dst) => src -> BS.ByteString -> dst -> BS.ByteString -> Maybe LCPL -> Maybe LAPL -> IO ()
copyLink  src srcName dst dstName lcpl lapl =
    withErrorCheck_ $
        BS.useAsCString srcName $ \srcName ->
            BS.useAsCString dstName $ \dstName ->
                h5l_copy (hid src) srcName (hid dst) dstName
                    (maybe h5p_DEFAULT hid lcpl)
                    (maybe h5p_DEFAULT hid lapl)

deleteLink :: Location t => t -> BS.ByteString -> Maybe LAPL -> IO ()
deleteLink loc name lapl =
    withErrorCheck_ $
        BS.useAsCString name $ \name ->
            h5l_delete (hid loc) name (maybe h5p_DEFAULT hid lapl)

data LinkType
    = External
    | Hard
    | Soft
    | OtherLinkType !H5L_type_t
    deriving (Eq, Ord, Read, Show)

linkTypeFromCode c
    | c == h5l_TYPE_EXTERNAL    = External
    | c == h5l_TYPE_HARD        = Hard
    | c == h5l_TYPE_SOFT        = Soft
    | c >= h5l_TYPE_UD_MIN      = OtherLinkType c
    | otherwise                 = error ("Unknown link type: " ++ show c)


data LinkInfo = LinkInfo
    { linkType          :: LinkType
    , linkCOrderValid   :: Bool
    , linkCOrder        :: Int64
    , linkCSet          :: CSet
    , linkAddress       :: HAddr
    , linkValSize       :: CSize
    } deriving (Eq, Ord, Read, Show)

readLinkInfo :: H5L_info_t -> LinkInfo
readLinkInfo i  = LinkInfo
    { linkType          = linkTypeFromCode (h5l_info_t'type i)
    , linkCOrderValid   = hboolToBool (h5l_info_t'corder_valid i)
    , linkCOrder        = h5l_info_t'corder i
    , linkCSet          = cSetFromCode (h5l_info_t'cset i)
    , linkAddress       = HAddr (h5l_info_t'u'address i)
    , linkValSize       = h5l_info_t'u'val_size i
    }

getLinkInfo :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO LinkInfo
getLinkInfo loc name lapl =
    fmap readLinkInfo $
        withOut_ $ \info ->
            withErrorCheck_ $
                BS.useAsCString name $ \name ->
                    h5l_get_info (hid loc) name info (maybe h5p_DEFAULT hid lapl)

getSymLinkVal :: Location loc => loc -> BS.ByteString -> Maybe LAPL -> IO BS.ByteString
getSymLinkVal loc name mb_lapl = 
    BS.useAsCString name $ \name -> do
        let lapl = maybe h5p_DEFAULT hid mb_lapl
        info <- withOut_ $ \info ->
            withErrorCheck_ $
                    h5l_get_info (hid loc) name info lapl
        
        let n = h5l_info_t'u'val_size info
        
        buf <- mallocBytes (fromIntegral n)
        
        withErrorCheck_ $
            h5l_get_val (hid loc) name (OutArray buf) n lapl
        -- TODO: this will leak memory if an exception is thrown
        
        BS.packCStringLen (buf, fromIntegral n)
        

foreign import ccall "wrapper" wrap_H5L_iterate_t
    :: (HId_t -> CString -> In H5L_info_t -> InOut a -> IO HErr_t)
    -> IO (FunPtr (HId_t -> CString -> In H5L_info_t -> InOut a -> IO HErr_t))

with_iterate_t :: (Group -> BS.ByteString -> LinkInfo -> IO HErr_t)
     -> (H5L_iterate_t () -> InOut () -> IO HErr_t)
     -> IO HErr_t
with_iterate_t op f = do
    exception <- newIORef Nothing :: IO (IORef (Maybe SomeException))
    
    op <- wrap_H5L_iterate_t $ \grp name (In link) _opData -> do
        name <- BS.packCString name
        link <- peek link
        result <- try (op (uncheckedFromHId grp) name (readLinkInfo link))
        case result of
            Left exc -> do
                writeIORef exception (Just exc)
                return maxBound
            Right x -> return x
    
    result <- f op (InOut nullPtr) `finally` freeHaskellFunPtr op
    
    if result == maxBound
        then do
            exception <- readIORef exception
            maybe (return result) throwIO exception
            
        else return result

iterateLinks :: Location t => t -> IndexType -> IterOrder -> Maybe HSize -> (Group -> BS.ByteString -> LinkInfo -> IO HErr_t) -> IO HSize
iterateLinks loc indexType order startIndex op = 
    fmap HSize $
        withInOut_ (maybe 0 hSize startIndex) $ \startIndex -> 
            withErrorCheck_ $
                with_iterate_t op $ \op opData -> 
                    h5l_iterate (hid loc) (indexTypeCode indexType) (iterOrderCode order) startIndex op opData

iterateLinksByName :: Location t => t -> BS.ByteString -> IndexType -> IterOrder -> Maybe HSize -> Maybe LAPL -> (Group -> BS.ByteString -> LinkInfo -> IO HErr_t) -> IO HSize
iterateLinksByName loc groupName indexType order startIndex lapl op = do
    fmap HSize $
        withInOut_ (maybe 0 hSize startIndex) $ \startIndex -> 
            withErrorCheck_ $
                with_iterate_t op $ \op opData ->
                    BS.useAsCString groupName $ \groupName ->
                        h5l_iterate_by_name (hid loc) groupName (indexTypeCode indexType) (iterOrderCode order) startIndex op opData (maybe h5p_DEFAULT hid lapl)

visitLinks :: Location t => t -> IndexType -> IterOrder -> (Group -> BS.ByteString -> LinkInfo -> IO HErr_t) -> IO ()
visitLinks loc indexType order op =
    withErrorCheck_ $
        with_iterate_t op $ \op opData ->
            h5l_visit (hid loc) (indexTypeCode indexType) (iterOrderCode order) op opData

visitLinksByName :: Location t => t -> BS.ByteString -> IndexType -> IterOrder -> Maybe LAPL -> (Group -> BS.ByteString -> LinkInfo -> IO HErr_t) -> IO ()
visitLinksByName loc groupName indexType order lapl op =
    withErrorCheck_ $
        with_iterate_t op $ \op opData ->
            BS.useAsCString groupName $ \groupName ->
                h5l_visit_by_name (hid loc) groupName (indexTypeCode indexType) (iterOrderCode order) op opData (maybe h5p_DEFAULT hid lapl)

