{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Error
    ( ErrorClassID, hdfError
    , HDF5Exception, errorStack, HDF5Error(..)
    , HDFResultType(..)
    , withErrorWhen, withErrorWhen_
    , withErrorCheck, withErrorCheck_
    , htriToBool
    , registerErrorClass, unregisterErrorClass
    , createMajorErrCode, releaseMajorErrCode
    , createMinorErrCode, releaseMinorErrCode
    , ErrorStack
    , createErrorStack, closeErrorStack
    , getCurrentErrorStack, setCurrentErrorStack,
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.ErrorCodes
import Bindings.HDF5.Raw.H5
import Bindings.HDF5.Raw.H5E
import Bindings.HDF5.Raw.H5I
import Control.Exception (throwIO, finally, Exception)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Typeable (Typeable)
import Foreign
import Foreign.C
import Foreign.Ptr.Conventions

newtype ErrorClassID = ErrorClassID HId_t
    deriving (Eq, Ord, Typeable, HId, FromHId, HDFResultType)

instance Show ErrorClassID where
    showsPrec p cls@(ErrorClassID (HId_t hid))
        | cls == hdfError
            = showString "hdfError"
        | otherwise = showsPrec p hid

hdfError :: ErrorClassID
hdfError = ErrorClassID h5e_ERR_CLS

data HDF5Error = HDF5Error
    { classId       :: !ErrorClassID
    , majorNum      :: !(Maybe MajorErrCode)
    , minorNum      :: !(Maybe MinorErrCode)
    , line          :: !Integer
    , funcName      :: !BS.ByteString
    , fileName      :: !BS.ByteString
    , description   :: !BS.ByteString
    } deriving (Eq, Ord, Show, Typeable)

readHDF5Error :: H5E_error2_t -> IO HDF5Error
readHDF5Error err = do
    func <- BS.packCString (h5e_error2_t'func_name err)
    file <- BS.packCString (h5e_error2_t'file_name err)
    desc <- BS.packCString (h5e_error2_t'desc err)
    
    return HDF5Error
        { classId       = ErrorClassID (h5e_error2_t'cls_id err)
        , majorNum      = majorErrorFromCode (h5e_error2_t'maj_num err)
        , minorNum      = minorErrorFromCode (h5e_error2_t'maj_num err)
        , line          = toInteger (h5e_error2_t'line err)
        , funcName      = func
        , fileName      = file
        , description   = desc
        }

newtype HDF5Exception = HDF5Exception [HDF5Error]
    deriving (Eq, Ord, Show, Typeable)
instance Exception HDF5Exception

errorStack :: HDF5Exception -> [HDF5Error]
errorStack (HDF5Exception es) = es

withErrorWhen :: (t -> Bool) -> IO t -> IO t
withErrorWhen isError action = do
    -- h5e_try does not alter the stack, just suspends the 'automatic' exception handler
    result <- h5e_try action
    
    if isError result
        then do
            stackId <- h5e_get_current_stack
            errors  <- newIORef []
            
            walk <- wrapStackWalk $ \_ (In err) _ -> do
                err_desc <- readHDF5Error =<< peek err
                modifyIORef errors (err_desc :)
                return (HErr_t 0)
            
            h5e_walk2 stackId h5e_WALK_DOWNWARD walk (InOut nullPtr)
                `finally` do
                    freeHaskellFunPtr walk
                    h5e_close_stack stackId
            
            errs <- readIORef errors
            throwIO (HDF5Exception errs)
        else return result

withErrorWhen_ :: (t -> Bool) -> IO t -> IO ()
withErrorWhen_ isError action = do
    withErrorWhen isError action
    return ()

withErrorCheck :: HDFResultType t => IO t -> IO t
withErrorCheck = withErrorWhen isError

withErrorCheck_ :: HDFResultType t => IO t -> IO ()
withErrorCheck_ = withErrorWhen_ isError

htriToBool :: IO HTri_t -> IO Bool
htriToBool = fmap toBool . withErrorCheck
    where toBool (HTri_t x) = x > 0

registerErrorClass :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO ErrorClassID
registerErrorClass name libName version =
    fmap ErrorClassID $
        withErrorCheck $
            BS.useAsCString name $ \name ->
                BS.useAsCString libName $ \libName ->
                    BS.useAsCString version $ \version ->
                        h5e_register_class name libName version

unregisterErrorClass :: ErrorClassID -> IO ()
unregisterErrorClass (ErrorClassID hid) = do
    withErrorCheck (h5e_unregister_class hid)
    return ()

createMajorErrCode :: ErrorClassID -> BS.ByteString -> IO MajorErrCode
createMajorErrCode (ErrorClassID cls) msg = 
    fmap UnknownMajor $
        withErrorCheck $
            BS.useAsCString msg $ \msg ->
                h5e_create_msg cls h5e_MAJOR msg

releaseMajorErrCode :: MajorErrCode -> IO ()
releaseMajorErrCode (UnknownMajor code) = do
    withErrorCheck (h5e_close_msg code)
    return ()
releaseMajorErrCode otherErr = fail $ concat
    [ "releaseMajorErrCode: "
    , show otherErr
    , " is a built-in error type, it's a bad idea to release it."
    ]

createMinorErrCode :: ErrorClassID -> BS.ByteString -> IO MinorErrCode
createMinorErrCode (ErrorClassID cls) msg = 
    fmap UnknownMinor $
        withErrorCheck $
            BS.useAsCString msg $ \msg ->
                h5e_create_msg cls h5e_MINOR msg

releaseMinorErrCode :: MinorErrCode -> IO ()
releaseMinorErrCode (UnknownMinor code) = do
    withErrorCheck (h5e_close_msg code)
    return ()
releaseMinorErrCode otherErr = fail $ concat
    [ "releaseMinorErrCode: "
    , show otherErr
    , " is a built-in error type, it's a bad idea to release it."
    ]

newtype ErrorStack = ErrorStack HId_t
    deriving (Eq, Ord, Show, HId, FromHId, HDFResultType)

createErrorStack :: IO ErrorStack
createErrorStack =
    fmap ErrorStack
        (withErrorCheck h5e_create_stack)

getCurrentErrorStack :: IO ErrorStack
getCurrentErrorStack =
    fmap ErrorStack
        (withErrorCheck h5e_get_current_stack)

setCurrentErrorStack :: ErrorStack -> IO ()
setCurrentErrorStack (ErrorStack hid) = do
    withErrorCheck (h5e_set_current_stack hid)
    return ()

closeErrorStack :: ErrorStack -> IO ()
closeErrorStack (ErrorStack hid) = do
    withErrorCheck (h5e_close_stack hid)
    return ()

foreign import ccall "wrapper" wrapStackWalk
    :: (CUInt -> In H5E_error2_t -> InOut a -> IO HErr_t)
    -> IO (FunPtr (CUInt -> In H5E_error2_t -> InOut a -> IO HErr_t))
