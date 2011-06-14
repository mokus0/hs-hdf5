{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList
    ( PropertyListClassID
    , root
    , objectCreate
    , fileCreate
    , fileAccess
    , datasetCreate
    , datasetAccess
    , datasetXfer
    , fileMount
    , groupCreate
    , groupAccess
    , datatypeCreate
    , datatypeAccess
    , stringCreate
    , attributeCreate
    , objectCopy
    , linkCreate
    , linkAccess
    
    , getClassName
    
    , PropertyListID
    , PropertyListOrClass(..)
    , PropertyList(..)
    , Tagged(..)
    , castPropertyList
    
    , createPropertyList
    , createPropertyListWithClass
    
    , propertyExists
    , getPropertySize
    , getNProps
    
    , getPropertyListClass
    , getPropertyListClassParent
    
    , propertyListsEqual
    
    , propertyListIsA
    
    , closePropertyListClass
    , closePropertyList
    ) where

import Bindings.HDF5.Raw.H5I
import Bindings.HDF5.Raw.H5P
import Bindings.HDF5.Core
import Bindings.HDF5.Error

import qualified Data.ByteString as BS
import Data.Tagged
import Foreign
import Foreign.C
import Foreign.Ptr.Conventions

newtype PropertyListClassID = PropertyListClassID HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

root                = PropertyListClassID h5p_ROOT              -- no parent
objectCreate        = PropertyListClassID h5p_OBJECT_CREATE     -- parent:  root
fileCreate          = PropertyListClassID h5p_FILE_CREATE       -- parent:  groupCreate
fileAccess          = PropertyListClassID h5p_FILE_ACCESS       -- parent:  root
datasetCreate       = PropertyListClassID h5p_DATASET_CREATE    -- parent:  objectCreate
datasetAccess       = PropertyListClassID h5p_DATASET_ACCESS    -- parent:  linkAccess
datasetXfer         = PropertyListClassID h5p_DATASET_XFER      -- parent:  root
fileMount           = PropertyListClassID h5p_FILE_MOUNT        -- parent:  root
groupCreate         = PropertyListClassID h5p_GROUP_CREATE      -- parent:  objectCreate
groupAccess         = PropertyListClassID h5p_GROUP_ACCESS      -- parent:  linkAccess
datatypeCreate      = PropertyListClassID h5p_DATATYPE_CREATE   -- parent:  objectCreate
datatypeAccess      = PropertyListClassID h5p_DATATYPE_ACCESS   -- parent:  linkAccess
stringCreate        = PropertyListClassID h5p_STRING_CREATE     -- parent:  root
attributeCreate     = PropertyListClassID h5p_ATTRIBUTE_CREATE  -- parent:  stringCreate
objectCopy          = PropertyListClassID h5p_OBJECT_COPY       -- parent:  root
linkCreate          = PropertyListClassID h5p_LINK_CREATE       -- parent:  stringCreate
linkAccess          = PropertyListClassID h5p_LINK_ACCESS       -- parent:  root

getClassName :: PropertyListClassID -> IO BS.ByteString
getClassName (PropertyListClassID cls) = do
    name <- withErrorWhen (nullPtr ==) $
        h5p_get_class_name cls
    
    nameStr <- BS.packCString name
    free name
    
    return nameStr

newtype PropertyListID = PropertyListID HId_t
    deriving (Eq, HId, FromHId, HDFResultType)

class (HId t, FromHId t) => PropertyListOrClass t where
class PropertyListOrClass t => PropertyList t where
    staticPlistClass :: Tagged t PropertyListClassID

instance PropertyListOrClass PropertyListID
instance PropertyListOrClass PropertyListClassID

instance PropertyList PropertyListID where
    staticPlistClass = Tagged root

uncheckedCastPlist :: (PropertyList a, PropertyList b) => a -> b
uncheckedCastPlist = uncheckedFromHId . hid

castPropertyList :: (PropertyList a, PropertyList b) => a -> IO (Maybe b)
castPropertyList = castTo staticPlistClass
    where
        castTo :: (PropertyList a, PropertyList b)
               => Tagged b PropertyListClassID -> a -> IO (Maybe b)
        castTo (Tagged cls) plist = do
            ok <- propertyListIsA plist cls
            if ok
                then return (Just . uncheckedCastPlist $ plist)
                else return Nothing

createPropertyList :: PropertyList t => IO t
createPropertyList = create staticPlistClass
    where
        create :: PropertyList t => Tagged t PropertyListClassID -> IO t
        create (Tagged cls) 
            = fmap uncheckedCastPlist
                $ createPropertyListWithClass cls

createPropertyListWithClass :: PropertyListClassID -> IO PropertyListID
createPropertyListWithClass (PropertyListClassID cls) = 
    fmap PropertyListID $
        withErrorCheck $
            h5p_create cls

propertyExists :: PropertyList t => t -> BS.ByteString -> IO Bool
propertyExists plist name =
    htriToBool $
        BS.useAsCString name $ \name ->
            h5p_exist (hid plist) name

getPropertySize :: PropertyListOrClass t => t -> BS.ByteString -> IO CSize
getPropertySize plist name = 
    withOut_ $ \sz ->
        withErrorCheck_ $
            BS.useAsCString name $ \name ->
                h5p_get_size (hid plist) name sz

getNProps :: PropertyListOrClass t => t -> IO CSize
getNProps plist = 
    withOut_ $ \sz ->
        withErrorCheck_ $
            h5p_get_nprops (hid plist) sz

getPropertyListClass :: PropertyList t => t -> IO PropertyListClassID
getPropertyListClass plist =
    fmap PropertyListClassID $
        withErrorCheck $
            h5p_get_class (hid plist)

getPropertyListClassParent :: PropertyListClassID -> IO PropertyListClassID
getPropertyListClassParent (PropertyListClassID cls) =
    fmap PropertyListClassID $
        withErrorCheck $
            h5p_get_class_parent cls

propertyListsEqual :: (PropertyListOrClass a, PropertyListOrClass b) => a -> b -> IO Bool
propertyListsEqual pl1 pl2 =
    htriToBool $
        h5p_equal (hid pl1) (hid pl2)

propertyListIsA :: PropertyList t => t -> PropertyListClassID -> IO Bool
propertyListIsA plist (PropertyListClassID cls) =
    htriToBool $
        h5p_isa_class (hid plist) cls

closePropertyListClass :: PropertyListClassID -> IO ()
closePropertyListClass (PropertyListClassID cls) =
    withErrorCheck_ $
        h5p_close_class cls

closePropertyList :: PropertyList t => t -> IO ()
closePropertyList plist =
    withErrorCheck_ $
        h5p_close (hid plist)
