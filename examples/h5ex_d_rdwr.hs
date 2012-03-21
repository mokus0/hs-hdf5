{-

  This example shows how to read and write data to a
  dataset.  The program first writes integers to a dataset
  with dataspace dimensions of DIM0xDIM1, then closes the
  file.  Next, it reopens the file, reads back the data, and
  outputs it to the screen.

  This file is intended for use with HDF5 Library version 1.8

-}

import Bindings.HDF5
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as SV
import Foreign.C.Types

filename = BS.pack "h5ex_d_rdwr.h5"
dataset  = BS.pack "DS1"
dim0 = 4
dim1 = 7

main = do
    -- Initialize data.

    let wdata :: SV.Vector CInt
        wdata = SV.fromList
            [ fromIntegral (i * j - j)
            | i <- [0..dim0-1]
            , j <- [0..dim1-1]
            ]
    putStrLn $ "wdata = " ++ show wdata

    -- Create a new file using the default properties.
    
    file <- createFile filename [Truncate] Nothing Nothing

    -- Create dataspace.  Setting maximum size to NULL sets the
    -- maximum size to be the current size.

    let dims = [dim0, dim1]
    space <- createSimpleDataspace dims

    -- Create the dataset.  We will use all default properties for
    -- this example.

    dset <- createDataset file dataset stdI32le space Nothing Nothing Nothing

    -- Write the data to the dataset.

    writeDataset dset Nothing Nothing Nothing wdata

    -- Close and release resources.

    closeDataset dset
    closeDataspace space
    closeFile file

    -- Now we begin the read section of this example.

    -- Open file and dataset using the default properties.

    file <- openFile filename [ReadOnly] Nothing
    dset <- openDataset file dataset Nothing

    -- Read the data using the default properties.
    rdata <- readDataset dset Nothing Nothing :: IO (SV.Vector CInt)

    -- Output the data to the screen.
    putStrLn $ "rdata " ++ show rdata

    --  Close and release resources.

    closeDataset dset
    closeFile file

    return ()