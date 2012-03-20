{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.OCPYPL
    ( module Bindings.HDF5.PropertyList
    
    , OCPYPL
    , ObjectCopyPropertyList(..)
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.PropertyList

class PropertyList t => ObjectCopyPropertyList t where
newtype OCPYPL = OCPYPL PropertyListID
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass)
instance PropertyList OCPYPL where
    staticPlistClass = Tagged objectCopy
instance ObjectCopyPropertyList OCPYPL
