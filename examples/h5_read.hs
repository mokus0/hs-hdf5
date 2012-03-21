module Main where

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 - Copyright by The HDF Group.                                               -
 - Copyright by the Board of Trustees of the University of Illinois.         -
 - All rights reserved.                                                      -
 -                                                                           -
 - This file is part of HDF5.  The full HDF5 copyright notice, including     -
 - terms governing use, modification, and redistribution, is contained in    -
 - the files COPYING and Copyright.html.  COPYING can be found at the root   -
 - of the source code distribution tree; Copyright.html can be found at the  -
 - root level of an installed copy of the electronic HDF5 document set and   -
 - is linked from the top-level documents page.  It can also be found at     -
 - http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          -
 - access to either file, you may request a copy from help@hdfgroup.org.     -
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-
 -   This example reads hyperslab from the SDS.h5 file
 -   created by h5_write.c program into two-dimensional
 -   plane of the three-dimensional array.
 -   Information about dataset in the SDS.h5 file is obtained.
 -}

import Bindings.HDF5
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SVM
import Foreign.C.Types

fileName    = BS.pack "SDS.h5"
datasetName = BS.pack "IntArray"
nxSub       = 3 -- hyperslab dimensions
nySub       = 4
nx          = 7 -- output buffer dimensions
ny          = 7
nz          = 3
rank        = 2
rankOut     = 3

main = do
    -- Open the file and the dataset.
    file <- openFile fileName [] Nothing
    dataset <- openDataset file datasetName Nothing
    
    -- Get datatype and dataspace handles and then query
    -- dataset class, order, size, rank and dimensions.
    datatype <- getDatasetType dataset
    t_class <- getTypeClass datatype
    putStrLn ("Data set has " ++ show t_class ++ " type")
    order <- getByteOrder datatype
    putStrLn ("Data set has byte order: " ++ show order)
    
    size <- getTypeSize datatype
    putStrLn ("Data size is " ++ show size)
    
    dataspace <- getDatasetSpace dataset
    rank <- getSimpleDataspaceExtentNDims dataspace
    (dims_out, _) <- getSimpleDataspaceExtent dataspace
    putStrLn ("rank " ++ show rank ++ ", dimensions " ++ show dims_out)
    
--  Define hyperslab in the dataset.
    selectHyperslab dataspace Set $
        [ (1, Nothing, nxSub, Nothing)
        , (2, Nothing, nySub, Nothing)
        ]
    
--  Define the memory dataspace.
    memspace <- createSimpleDataspace [nx, ny, nz]
    
--  Define memory hyperslab.
    selectHyperslab memspace Set $
        [ (3, Nothing, nxSub, Nothing)
        , (0, Nothing, nySub, Nothing)
        , (0, Nothing, 1,     Nothing)
        ]
    
--  Read data from hyperslab in the file into the hyperslab in
--  memory and display.
    data_out <- SVM.replicate (fromIntegral (nx*ny*nz)) (0 :: CInt)
    readDatasetInto dataset (Just memspace) (Just dataspace) Nothing data_out
    data_out <- SV.freeze data_out
    
    putStrLn "data:"
    sequence_
        [ do
            mapM_ print (chunks ny plane)
            putStrLn ""
        | plane <- slabs nz (SV.toList data_out)
        ]
    
--  Close/release resources.
    closeTypeID datatype
    closeDataset dataset
    closeDataspace dataspace
    closeDataspace memspace
    closeFile file

    return ()

-- some convenience functions for working with flattened multidimensional lists
chunks n xs = case splitAt (fromIntegral n) xs of (a,[]) -> [a]; (a,b) -> a : chunks n b
slabs n = transpose . chunks n