{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.FMPL
    ( module Bindings.HDF5.PropertyList
    
    , FMPL
    , FileMountPropertyList(..)
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.PropertyList

class PropertyList t => FileMountPropertyList t where
newtype FMPL = FMPL PropertyListID
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass)
instance PropertyList FMPL where
    staticPlistClass = Tagged fileMount
instance FileMountPropertyList FMPL
