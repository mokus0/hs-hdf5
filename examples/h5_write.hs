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
 -  This example writes data to the HDF5 file.
 -  Data conversion is performed during write operation.
 -}

import Bindings.HDF5
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as SV
import Foreign.C.Types

fileName    = BS.pack "SDS.h5"
datasetName = BS.pack "IntArray"
nx = 5  -- dataset dimensions
ny = 6
rank = 2

-- int
main = do
    -- Data  and output buffer initialization.
    -- TODO: pick (or kludge together) a multidimensional array interface.
    --       Currently I'm thinking REPA, despite the fact that copying will
    --       probably be necessary.  A nice 'repa-ffi' interface would be
    --       nifty.  Even if it operated by copying, if it were shown to be
    --       useful enough, REPA could perhaps be extended to support non-
    --       copying FFI access.
    let data_ :: SV.Vector CInt
        data_ = SV.fromList
            [ i+j
            | j <- [0 .. fromIntegral nx - 1]
            , i <- [0 .. fromIntegral ny - 1]
            ]
    
    -- Create a new file using H5F_ACC_TRUNC access,
    -- default file creation properties, and default file
    -- access properties.
    file <- createFile fileName [Truncate] Nothing Nothing
    
    -- Describe the size of the array and create the data space for fixed
    -- size dataset.
    
    let dimsf = [nx, ny]
    dataspace <- createSimpleDataspace dimsf
    
    -- Define datatype for the data in the file.
    -- We will store little endian INT numbers.
    
    datatype <- copyTypeID nativeInt
    setByteOrder datatype (Just LE)
    
    -- Create a new dataset within the file using defined dataspace and
    -- datatype and default dataset creation properties.
    dataset <- createDataset file datasetName datatype dataspace
                    Nothing Nothing Nothing
    
    -- Write the data to the dataset using default transfer properties.
    writeDataset dataset Nothing Nothing Nothing data_
    
    -- Close/release resources.
    closeDataspace dataspace
    closeTypeID datatype
    closeDataset dataset
    closeFile file
    
    return ()
