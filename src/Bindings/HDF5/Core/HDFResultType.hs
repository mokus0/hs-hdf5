{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Core.HDFResultType where

import Bindings.HDF5.Raw

-- |Types returned from HDF5 library functions which always signal failure
-- the same way.
class HDFResultType t where
    isError :: t -> Bool

instance HDFResultType HId_t where
    isError = (< HId_t 0)

instance HDFResultType HErr_t where
    isError = (< 0)

instance HDFResultType HTri_t where
    isError (HTri_t x) = x < 0

instance HDFResultType HAddr_t where
    isError = (hADDR_UNDEF ==)

instance HDFResultType H5T_order_t where
    isError (H5T_order_t c) = c < 0
    
instance HDFResultType H5T_pad_t where
    isError (H5T_pad_t c) = c < 0

instance HDFResultType H5T_str_t where
    isError (H5T_str_t c) = c < 0
