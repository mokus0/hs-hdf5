{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.PropertyList.DXPL
    ( module Bindings.HDF5.PropertyList
    
    , DXPL
    , DatasetTransferPropertyList(..)
    
    ) where

import Bindings.HDF5.Core
import Bindings.HDF5.PropertyList

class PropertyList t => DatasetTransferPropertyList t where
newtype DXPL = DXPL PropertyListID
    deriving (Eq, HId, FromHId, HDFResultType, PropertyListOrClass)
instance PropertyList DXPL where
    staticPlistClass = Tagged datasetXfer
instance DatasetTransferPropertyList DXPL

-- TODO: implement functions

