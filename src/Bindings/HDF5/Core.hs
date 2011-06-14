{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bindings.HDF5.Core
    ( module Bindings.HDF5.Core
    , module Bindings.HDF5.Core.HId
    , module Bindings.HDF5.Core.HDFResultType
    ) where

import Bindings.HDF5.Raw.H5

import Bindings.HDF5.Core.HId
import Bindings.HDF5.Core.HDFResultType

import Data.Bits
import Foreign.Storable

newtype HSize = HSize HSize_t
    deriving (Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, Storable)

hSize (HSize s) = s

instance Read HSize where
    readsPrec p s =
        [ (HSize (HSize_t n), rest)
        | (n,rest) <- readsPrec p s
        ]

instance Show HSize where
    showsPrec p (HSize (HSize_t n)) = showsPrec p n

newtype HSSize = HSSize HSSize_t
    deriving (Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, Storable)

hSSize (HSSize s) = s

instance Read HSSize where
    readsPrec p s =
        [ (HSSize (HSSize_t n), rest)
        | (n,rest) <- readsPrec p s
        ]

instance Show HSSize where
    showsPrec p (HSSize (HSSize_t n)) = showsPrec p n

data IH_Info = IH_Info
    { indexSize     :: !HSize
    , heapSize      :: !HSize
    } deriving (Eq, Ord, Read, Show)

data IndexType
    = ByName
    | ByCreationOrder
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance HDFResultType H5_index_t where
    isError (H5_index_t c) = c < 0

indexTypeCode :: IndexType -> H5_index_t
indexTypeCode ByName            = h5_INDEX_NAME
indexTypeCode ByCreationOrder   = h5_INDEX_CRT_ORDER

data IterOrder
    = Increasing
    | Decreasing
    | Native
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance HDFResultType H5_iter_order_t where
    isError (H5_iter_order_t c) = c < 0

iterOrderCode :: IterOrder -> H5_iter_order_t
iterOrderCode Increasing = h5_ITER_INC
iterOrderCode Decreasing = h5_ITER_DEC
iterOrderCode Native     = h5_ITER_NATIVE
