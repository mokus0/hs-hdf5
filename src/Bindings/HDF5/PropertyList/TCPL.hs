{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.TCPL
    ( module Bindings.HDF5.PropertyList.OCPL
    
    , TCPL
    , DatatypeCreationPropertyList(..)
    
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.PropertyList.OCPL

newtype TCPL = TCPL OCPL
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass, ObjectCreationPropertyList)

instance PropertyList TCPL where
    staticPlistClass = Tagged datatypeCreate

class ObjectCreationPropertyList t => DatatypeCreationPropertyList t where
instance DatatypeCreationPropertyList TCPL

