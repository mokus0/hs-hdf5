{-

This example shows how to create and extend an unlimited dataset.

The program first writes integers to a dataset with dataspace
dimensions of DIM0xDIM1, then closes the file.  Next, it reopens the
file, reads back the data, outputs it to the screen, extends the
dataset, and writes new data to the extended portions of the dataset.
Finally it reopens the file again, reads back the data, and outputs it
to the screen.

This file is intended for use with HDF5 Library version 1.8

-}

import Bindings.HDF5
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as SV
import Foreign.C.Types

filename = BS.pack "h5ex_d_unlimadd.h5"
dataset  = BS.pack "DS1"
dim0 = 4
dim1 = 7
edim0 = 6
edim1 = 10
chunk0 = 4
chunk1 = 4

{-

Some things of interest

using CInt and HDF - NATIVE should always do the right thing.

NULL can be used as an argument for a lot of functions in place of a
pointer to data.    Using nullPtr will not work, it will need to be
wrapped with an InArray typecast, e.g.

(InArray nullPtr)

-}

main = do
    -- Initialize data.

    let wdata :: SV.Vector CInt
        wdata = SV.fromList
            [ fromIntegral (i * j - j)
            | i <- [0..dim0-1]
            , j <- [0..dim1-1]
            ]
    
    putStrLn "dim0, dim1"
    putStrLn $ show dim0 ++ " " ++ show dim1
    putStrLn "wdata"
    putStrLn $ show wdata

    -- Create a new file using the default properties.
                    
    file <- createFile filename [Truncate] Nothing Nothing

    -- Create dataspace with unlimited dimensions.

    space <- createExpandableDataspace [(dim0, Nothing), (dim1, Nothing)]

    -- Create the dataset creation property list, and set the chunk
    -- size.

    putStrLn $ "chunk0,chunk1 " ++ show chunk0 ++ "," ++ show chunk1
    dcpl <- createPropertyList
    setChunk dcpl [ chunk0, chunk1 ]

    -- Create the unlimited dataset.

    dset <- createDataset file dataset nativeInt space Nothing (Just dcpl) Nothing

    -- Write the data to the dataset.

    writeDataset dset Nothing Nothing Nothing wdata

    -- Close and release resources.

    closePropertyList dcpl
    closeDataset dset
    closeDataspace space
    closeFile file

    -- In this next section we read back the data, extend the dataset,
    -- and write new data to the extended portions.

    -- Open file and dataset using the default properties.

    file <- openFile filename [ReadWrite] Nothing
    dset <- openDataset file dataset Nothing

    -- Get dataspace and allocate memory for read buffer.    This is a
    -- two dimensional dataset so the dynamic allocation must be done
    -- in steps.

    space <- getDatasetSpace dset

    (dims, _) <- getSimpleDataspaceExtent space

    -- Read the data using the default properties.
    rdata <- readDataset dset Nothing Nothing :: IO (SV.Vector CInt)

    -- Output the data to the screen.

    putStrLn    "Dataset before extension:\n"
    print rdata

    closeDataspace space

    -- Extend the dataset.

    setDatasetExtent dset [ edim0, edim1 ]
        
    -- Retrieve the dataspace for the newly extended dataset.

    space <- getDatasetSpace dset

    -- Initialize data for writing to the extended dataset.

    let wdata2 :: SV.Vector CInt
        wdata2 = SV.fromList
            [ fromIntegral j
            | i <- [0..edim0-1], j <- [0..edim1-1]
            ]

    putStrLn "wdata2"
    print wdata2

    -- Select the entire dataspace.

    selectAll space
                            
    -- Subtract a hyperslab reflecting the original dimensions from the
    -- selection.    The selection now contains only the newly extended
    -- portions of the dataset.

    selectHyperslab space NotB 
        [ (0, Nothing, dims !! 0, Nothing)
        , (0, Nothing, dims !! 1, Nothing)
        ]

    -- Write the data to the selected portion of the dataset.

    writeDataset dset Nothing (Just space) Nothing wdata2

    -- Close and release resources.

    closeDataset dset
    closeDataspace space
    closeFile file

    -- Now we simply read back the data and output it to the screen.

    -- Open file and dataset using the default properties.

    file <- openFile filename [ReadOnly] Nothing
    dset <- openDataset file dataset Nothing

    -- Get dataspace and allocate memory for the read buffer as before.

    space <- getDatasetSpace dset

    (dims, _) <- getSimpleDataspaceExtent space

    rdata <- readDataset dset Nothing Nothing :: IO (SV.Vector CInt)

    -- Output the data to the screen.

    putStrLn "\nDataset after extension:\n"
    putStrLn $ show rdata

    -- Close and release resources.

    closeDataset dset
    closeDataspace space
    closeFile file

    return ()
