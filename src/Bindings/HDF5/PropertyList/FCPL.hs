{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.FCPL
    ( module Bindings.HDF5.PropertyList.GCPL
    
    , FCPL
    , FileCreationPropertyList(..)
    
    , setUserblock
    , getUserblock
    
    , setSizes
    , getSizes
    
    , setSymK
    , getSymK
    
    , setIstoreK
    , getIstoreK
    
    , setSharedMesgNIndexes
    , getSharedMesgNIndexes
    
    , setSharedMesgIndex
    , getSharedMesgIndex
    
    , setSharedMesgPhaseChange
    , getSharedMesgPhaseChange
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.PropertyList.GCPL
import Bindings.HDF5.Raw.H5P
import Data.Maybe
import Foreign.C.Types
import Foreign.Ptr.Conventions

newtype FCPL = FCPL GCPL
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass, ObjectCreationPropertyList, GroupCreationPropertyList)

instance PropertyList FCPL where
    staticPlistClass = Tagged fileCreate

class GroupCreationPropertyList t => FileCreationPropertyList t where
instance FileCreationPropertyList FCPL

-- TODO: figure out what the Out ptrs here represent.  Are they arrays?
-- getVersion :: FileCreationPropertyList fcpl => fcpl -> ...

setUserblock :: FileCreationPropertyList fcpl => fcpl -> HSize -> IO ()
setUserblock fcpl sz =
    withErrorCheck_ $
        h5p_set_userblock (hid fcpl) (hSize sz)

getUserblock :: FileCreationPropertyList fcpl => fcpl -> IO HSize
getUserblock fcpl =
    fmap HSize $
        withOut_ $ \sz -> 
            withErrorCheck_ $
                h5p_get_userblock (hid fcpl) sz

setSizes :: FileCreationPropertyList fcpl => fcpl -> CSize -> CSize -> IO ()
setSizes fcpl sizeof_addr sizeof_size =
    withErrorCheck_ $
        h5p_set_sizes (hid fcpl) sizeof_addr sizeof_size

getSizes :: FileCreationPropertyList fcpl => fcpl -> IO (CSize, CSize)
getSizes fcpl =
    withOut $ \sizeof_addr ->
        withOut_ $ \sizeof_size ->
            withErrorCheck_ $
                h5p_get_sizes (hid fcpl) sizeof_addr sizeof_size

setSymK :: FileCreationPropertyList fcpl => fcpl -> Maybe CUInt -> Maybe CUInt -> IO ()
setSymK fcpl mbIK mbLK =
    withErrorCheck_ $
        h5p_set_sym_k (hid fcpl) (fromMaybe 0 mbIK) (fromMaybe 0 mbLK)

getSymK :: FileCreationPropertyList fcpl => fcpl -> IO (CUInt, CUInt)
getSymK fcpl = 
    withOut $ \ik ->
        withOut_ $ \lk ->
            withErrorCheck_ $
                h5p_get_sym_k (hid fcpl) ik lk

setIstoreK :: FileCreationPropertyList fcpl => fcpl -> CUInt -> IO ()
setIstoreK fcpl ik =
    withErrorCheck_ $
        h5p_set_istore_k (hid fcpl) ik

getIstoreK :: FileCreationPropertyList fcpl => fcpl -> IO CUInt
getIstoreK fcpl =
    withOut_ $ \ik -> 
        withErrorCheck_ $
            h5p_get_istore_k (hid fcpl) ik

setSharedMesgNIndexes :: FileCreationPropertyList fcpl => fcpl -> CUInt -> IO ()
setSharedMesgNIndexes fcpl nIndexes =
    withErrorCheck_ $
        h5p_set_shared_mesg_nindexes (hid fcpl) nIndexes

getSharedMesgNIndexes :: FileCreationPropertyList fcpl => fcpl -> IO CUInt
getSharedMesgNIndexes fcpl =
    withOut_ $ \nIndexes ->
        withErrorCheck_ $
            h5p_get_shared_mesg_nindexes (hid fcpl) nIndexes

-- TODO: figure out what, if any, constants apply for mesgTypeFlags
setSharedMesgIndex :: FileCreationPropertyList fcpl => fcpl -> CUInt -> CUInt -> CUInt -> IO ()
setSharedMesgIndex fcpl indexNum mesgTypeFlags minMesgSize =
    withErrorCheck_ $
        h5p_set_shared_mesg_index (hid fcpl) indexNum mesgTypeFlags minMesgSize

getSharedMesgIndex :: FileCreationPropertyList fcpl => fcpl -> CUInt -> IO (CUInt, CUInt)
getSharedMesgIndex fcpl indexNum =
    withOut $ \mesgTypeFlags ->
        withOut_ $ \minMesgSize ->
            withErrorCheck_ $
                h5p_get_shared_mesg_index (hid fcpl) indexNum mesgTypeFlags minMesgSize

setSharedMesgPhaseChange :: FileCreationPropertyList fcpl => fcpl -> CUInt -> CUInt -> IO ()
setSharedMesgPhaseChange fcpl maxList minBTree =
    withErrorCheck_ $
        h5p_set_shared_mesg_phase_change (hid fcpl) maxList minBTree

getSharedMesgPhaseChange :: FileCreationPropertyList fcpl => fcpl -> IO (CUInt, CUInt)
getSharedMesgPhaseChange fcpl =
    withOut $ \maxList ->
        withOut_ $ \minBTree ->
            withErrorCheck_ $
                h5p_get_shared_mesg_phase_change (hid fcpl) maxList minBTree

