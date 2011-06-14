{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.GAPL
    ( module Bindings.HDF5.PropertyList.LAPL
    
    , GAPL
    , GroupAccessPropertyList(..)
    
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.PropertyList.LAPL

class LinkAccessPropertyList t => GroupAccessPropertyList t where
newtype GAPL = GAPL LAPL
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass, LinkAccessPropertyList)
instance PropertyList GAPL where
    staticPlistClass = Tagged groupAccess
instance GroupAccessPropertyList GAPL

-- TODO: implement functions

