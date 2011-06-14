{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |The HDF5 C type @hid_t@ is used to represent many, many different things.  
-- In this Haskell interface, it is always wrapped up in a newtype
-- indicating its intent.  These newtypes all need to be convertible to
-- and from 'HId_t' in order to actually use them with the raw bindings.
-- This modules provides a standard interface for doing so.
module Bindings.HDF5.Core.HId where

import Bindings.HDF5.Raw.H5I

-- |Types which can be converted to 'HId_t' (i.e., those which can be passed
-- to HDF5 functions)
class HId t where
    hid         :: t -> HId_t

-- |Types which can be converted from 'HId_t' (i.e., those which can be
-- returned by HDF5 functions)
class FromHId t where
    uncheckedFromHId :: HId_t -> t

instance HId HId_t where
    hid = id

instance FromHId HId_t where
    uncheckedFromHId = id

-- |HId types which can also serve as containers for other objects
class HId t => Location t where
