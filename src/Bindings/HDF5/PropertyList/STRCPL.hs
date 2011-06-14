{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.STRCPL
    ( module Bindings.HDF5.PropertyList
    
    , STRCPL
    , StringCreationPropertyList(..)
    
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.PropertyList

class PropertyList t => StringCreationPropertyList t where
newtype STRCPL = STRCPL PropertyListID
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass)
instance PropertyList STRCPL where
    staticPlistClass = Tagged stringCreate
instance StringCreationPropertyList STRCPL

-- TODO: implement functions

