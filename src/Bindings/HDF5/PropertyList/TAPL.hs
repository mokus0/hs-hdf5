{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.TAPL
    ( module Bindings.HDF5.PropertyList.LAPL
    
    , TAPL
    , DatatypeAccessPropertyList(..)
    
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.PropertyList.LAPL

newtype TAPL = TAPL LAPL
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass, LinkAccessPropertyList)

instance PropertyList TAPL where
    staticPlistClass = Tagged datatypeAccess

class LinkAccessPropertyList t => DatatypeAccessPropertyList t where
instance DatatypeAccessPropertyList TAPL

