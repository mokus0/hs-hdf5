{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Bindings.HDF5

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as SV

import Foreign.C.Types (CInt)

testFile = BS.pack "test_filters.h5"
dsetName = BS.pack "dset"

dimSizes, dimChunkSizes :: Num a => [a]
dimSizes = [20]
dimChunkSizes = [10]

dsetContents :: SV.Vector CInt
dsetContents = SV.replicate (product dimSizes) 1

test_filters_endianess = do
    -- create a file using default properties
    withNewFile testFile [Truncate] Nothing Nothing $ \fid -> do
        -- create a data space */
        withNewSimpleDataspace dimSizes $ \sid -> do
            -- create dcpl
            withNewPList $ \dcpl -> do
                setChunk dcpl dimChunkSizes                
                setFletcher32 dcpl
                
                -- create a dataset
                withNewDataset fid dsetName nativeInt sid Nothing (Just dcpl) Nothing $ \dsid -> do
                    writeDataset dsid Nothing Nothing Nothing dsetContents

withNewFile path mode pl1 pl2 = bracket
    (createFile path mode pl1 pl2)
    closeFile

withNewSimpleDataspace dims = bracket
    (createSimpleDataspace dims)
    closeDataspace

withNewPList = bracket createPropertyList closePropertyList

withNewDataset fid name ty sid pl1 pl2 pl3 = bracket
    (createDataset fid name ty sid pl1 pl2 pl3)
    closeDataset

main = do
    result <- test_filters_endianess
    return ()
