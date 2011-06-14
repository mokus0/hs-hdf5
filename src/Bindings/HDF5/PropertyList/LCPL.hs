{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.LCPL
    ( module Bindings.HDF5.PropertyList.STRCPL
    
    , LCPL
    , LinkCreationPropertyList(..)
    
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.PropertyList.STRCPL

class StringCreationPropertyList t => LinkCreationPropertyList t where
newtype LCPL = LCPL STRCPL
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass, StringCreationPropertyList)
instance PropertyList LCPL where
    staticPlistClass = Tagged linkCreate
instance LinkCreationPropertyList LCPL

-- TODO: implement functions

