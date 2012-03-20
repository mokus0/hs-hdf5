{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Bindings.HDF5.File
    ( AccFlags(..)
    , ObjType(..)
    , Scope(..)
    
    , isHDF5
    
    , File
    , createFile
    , openFile
    , reopenFile
    , flushFile
    , closeFile
    
    , mountFile
    , unmountFile
     
    , getFileSize
    , getFileCreatePlist
    , getFileAccessPlist
    
    , FileInfo(..)
    , getFileInfo
    , getFileIntent
    , getFileName
    , getFileObjCount
        
    , getOpenObjects
    , getFileFreespace
--    , get_mdc_config
--    , get_mdc_hit_rate
--    , get_mdc_size
--    , clear_elink_file_cache
--    , reset_mdc_hit_rate_stats
--    , set_mdc_config
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.Error
import Bindings.HDF5.Object
import Bindings.HDF5.PropertyList.FAPL
import Bindings.HDF5.PropertyList.FCPL
import Bindings.HDF5.PropertyList.FMPL
import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5F
import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5P
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as SV
import Foreign.C
import Foreign.Ptr
import Foreign.Ptr.Conventions
import Foreign.Storable

-- TODO: determine whether all of these are valid for _both_ create and open.
-- any that are not should be converted to Bool inputs to the corresponding function.
-- It very well may be best to separate several of them out like that anyway.
data AccFlags
    = ReadOnly
    | ReadWrite
    | Truncate
    | FailIfExists
    | Debug
    | Create
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

accFlagToInt :: AccFlags -> CUInt
accFlagToInt ReadOnly       = h5f_ACC_RDONLY
accFlagToInt ReadWrite      = h5f_ACC_RDWR
accFlagToInt Truncate       = h5f_ACC_TRUNC
accFlagToInt FailIfExists   = h5f_ACC_EXCL
accFlagToInt Debug          = h5f_ACC_DEBUG
accFlagToInt Create         = h5f_ACC_CREAT

accFlagsToInt :: [AccFlags] -> CUInt
accFlagsToInt = foldl (.|.) 0 . map accFlagToInt

intToAccFlags :: CUInt -> [AccFlags]
intToAccFlags x =
    [ f
    | f <- [minBound .. maxBound]
    , accFlagToInt f .&. x /= 0
    ]

instance Storable [AccFlags] where
    sizeOf    _ = sizeOf (0 :: CUInt)
    alignment _ = alignment (0 :: CUInt)
    peek        = fmap intToAccFlags . peek . castPtr
    poke p      = poke (castPtr p) . accFlagsToInt

data ObjType
    = Files
    | Datasets
    | Groups
    | Datatypes
    | Attrs
    | All
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

objTypeToInt :: ObjType -> CUInt
objTypeToInt Files      = h5f_OBJ_FILE
objTypeToInt Datasets   = h5f_OBJ_DATASET
objTypeToInt Groups     = h5f_OBJ_GROUP
objTypeToInt Datatypes  = h5f_OBJ_DATATYPE
objTypeToInt Attrs      = h5f_OBJ_ATTR
objTypeToInt All        = h5f_OBJ_ALL

objTypesToInt :: [ObjType] -> CUInt
objTypesToInt = foldl (.|.) 0 . map objTypeToInt

intToObjTypes :: CUInt -> [ObjType]
intToObjTypes x =
    [ f
    | f <- [minBound .. maxBound]
    , objTypeToInt f .&. x /= 0
    ]

instance Storable [ObjType] where
    sizeOf    _ = sizeOf (0 :: CUInt)
    alignment _ = alignment (0 :: CUInt)
    peek        = fmap intToObjTypes . peek . castPtr
    poke p      = poke (castPtr p) . objTypesToInt

data Scope
    = Local
    | Global
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

scopeCode Local  = h5f_SCOPE_LOCAL
scopeCode Global = h5f_SCOPE_GLOBAL

data CloseDegree
    = Weak
    | Semi
    | Strong
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

rawCloseDegreesInv = [(a,b) | (b,a) <- rawCloseDegrees]
rawCloseDegrees =
    [ (Nothing,     h5f_CLOSE_DEFAULT)
    , (Just Weak,   h5f_CLOSE_WEAK)
    , (Just Semi,   h5f_CLOSE_SEMI)
    , (Just Strong, h5f_CLOSE_STRONG)
    ]

closeDegreeFromCode :: H5F_close_degree_t -> Maybe CloseDegree
closeDegreeFromCode c = maybe Nothing id (lookup c rawCloseDegreesInv)

closeDegreeToCode :: Maybe CloseDegree -> H5F_close_degree_t
closeDegreeToCode c = case lookup c rawCloseDegrees of
    Nothing -> error ("closeDegreeToCode: unrecognized H5F_close_degree_t: " ++ show c)
    Just d  -> d

instance Storable (Maybe CloseDegree) where
    sizeOf _    = sizeOf    (undefined :: H5F_close_degree_t)
    alignment _ = alignment (undefined :: H5F_close_degree_t)
    peek        = fmap closeDegreeFromCode . peek . castPtr
    poke p      = poke (castPtr p) . closeDegreeToCode

isHDF5 :: BS.ByteString -> IO Bool
isHDF5 filename = htriToBool (BS.useAsCString filename h5f_is_hdf5)

newtype File = File HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

instance Location File
instance Object File where
    staticObjectType = Tagged (Just FileObj)

createFile :: BS.ByteString -> [AccFlags] -> Maybe FCPL -> Maybe FAPL -> IO File
createFile filename flags create_plist access_plist =
    fmap File $
        withErrorCheck $
            BS.useAsCString filename $ \filename ->
                h5f_create filename (accFlagsToInt flags) (maybe h5p_DEFAULT hid create_plist) (maybe h5p_DEFAULT hid access_plist)

openFile :: BS.ByteString -> [AccFlags] -> Maybe FAPL -> IO File
openFile filename flags access_plist =
    fmap File $
        withErrorCheck $
            BS.useAsCString filename $ \filename ->
                h5f_open filename (accFlagsToInt flags) (maybe h5p_DEFAULT hid access_plist)

reopenFile :: File -> IO File
reopenFile (File file_id) = 
    fmap File $
        withErrorCheck $
            h5f_reopen file_id

flushFile :: File -> Scope -> IO ()
flushFile (File file_id) scope =
    withErrorCheck_ $ 
        h5f_flush file_id (scopeCode scope)

closeFile :: File -> IO ()
closeFile (File file_id) =
    withErrorCheck_ (h5f_close file_id)

mountFile :: Location loc => loc -> BS.ByteString -> File -> Maybe FMPL -> IO ()
mountFile loc groupname (File file_id) mount_plist = 
    withErrorCheck_ $
        BS.useAsCString groupname $ \groupname ->
            h5f_mount (hid loc) groupname file_id (maybe h5p_DEFAULT hid mount_plist)

unmountFile :: Location loc => loc -> BS.ByteString -> IO ()
unmountFile loc groupname = 
    withErrorCheck_ $
        BS.useAsCString groupname $ \groupname ->
            h5f_unmount (hid loc) groupname

getFileSize :: File -> IO HSize
getFileSize (File file_id) =
    fmap HSize $
        withOut_ $ \sz ->
            withErrorCheck $
                h5f_get_filesize file_id sz

getFileCreatePlist :: File -> IO FCPL
getFileCreatePlist (File file_id) =
    fmap uncheckedFromHId $
        withErrorCheck $
            h5f_get_create_plist file_id

getFileAccessPlist :: File -> IO FAPL
getFileAccessPlist (File file_id) =
    fmap uncheckedFromHId $
        withErrorCheck $
            h5f_get_access_plist file_id

data FileInfo = FileInfo
    { superExtSize  :: !HSize
    , sohmHdrSize   :: !HSize
    , sohmMsgsInfo  :: !IH_Info
    } deriving (Eq, Ord, Read, Show)

readFileInfo :: H5F_info_t -> FileInfo
readFileInfo (H5F_info_t a b (H5_ih_info_t c d)) = FileInfo (HSize a) (HSize b) (IH_Info (HSize c) (HSize d))

getFileInfo :: Object obj => obj -> IO FileInfo
getFileInfo obj = 
    fmap readFileInfo $
        withOut_ $ \info ->
            withErrorCheck $
                h5f_get_info (hid obj) info

getFileIntent :: File -> IO [AccFlags]
getFileIntent (File file_id) = 
    fmap intToAccFlags $
        withOut_ $ \intent ->
            withErrorCheck_ $
                h5f_get_intent file_id intent

getFileName :: File -> IO BS.ByteString
getFileName (File file_id) = 
    withOutByteString $ \buf bufSz ->
        withErrorWhen (< 0) $
            h5f_get_name file_id buf bufSz

getFileObjCount :: Maybe File -> Bool -> [ObjType] -> IO CSize
getFileObjCount mbFile local objTypes =
    fmap fromIntegral $
        withErrorWhen (< 0) $
            h5f_get_obj_count (maybe (HId_t h5f_OBJ_ALL) hid mbFile) (objTypesToInt objTypes .|. if local then 0 else h5f_OBJ_LOCAL)

getOpenObjects :: Maybe File -> Bool -> [ObjType] -> IO (SV.Vector ObjectId)
getOpenObjects mbFile local objTypes = do
    n <- getFileObjCount mbFile local objTypes
    
    withOutVector' (fromIntegral n) $ \objects ->
        withErrorWhen (< 0) $
            h5f_get_obj_ids (maybe (HId_t h5f_OBJ_ALL) hid mbFile) (objTypesToInt objTypes .|. if local then 0 else h5f_OBJ_LOCAL) n (castWrappedPtr objects)

getFileFreespace :: File -> IO HSize
getFileFreespace (File file_id) =
    fmap fromIntegral $
        withErrorWhen (< 0) $
            h5f_get_freespace file_id
