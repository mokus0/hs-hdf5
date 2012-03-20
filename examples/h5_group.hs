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
 - This program creates a group in the file and two datasets in the group.
 - Hard link to the group object is created and one of the datasets is accessed
 - under new name.
 - Iterator functions are used to find information about the objects
 - in the root group and in the created group.
 -}

import Bindings.HDF5.Core
import Bindings.HDF5.Dataset
import Bindings.HDF5.Dataspace
import Bindings.HDF5.Datatype
import Bindings.HDF5.File
import Bindings.HDF5.Group
import Bindings.HDF5.Link
import Bindings.HDF5.PropertyList.DCPL

import qualified Data.ByteString.Char8 as BS

fileName = BS.pack "group.h5"
rank = 2

main = do
    -- Create a file.
    file <- createFile fileName [Truncate] Nothing Nothing

    -- Create a group in the file.
    grp <- createGroup file (BS.pack "/Data") Nothing Nothing Nothing
    
    -- Create dataset "Compressed Data" in the group using absolute
    -- name. Dataset creation property list is modified to use
    -- GZIP compression with the compression effort set to 6.
    -- Note that compression can be used only when dataset is chunked.
    
    let dims  = [1000, 20]
        cdims = [  20, 20]
    dataspace <- createSimpleDataspace dims
    plist <- createPropertyList
    setChunk plist cdims
    setDeflate plist 6
    
    dataset <- createDataset file (BS.pack "/Data/Compressed_Data") nativeInt dataspace Nothing (Just plist) Nothing
    
    -- Close the first dataset .
    closeDataspace dataspace
    closeDataset dataset
    
    -- Create the second dataset.
    let dims = [500,20]
    dataspace <- createSimpleDataspace dims
    dataset <- createDataset file (BS.pack "/Data/Float_Data") nativeFloat dataspace Nothing Nothing Nothing
    
    -- Close the second dataset and file.
    closeDataspace dataspace
    closeDataset dataset
    closePropertyList plist
    closeGroup grp
    closeFile file
    
    -- Now reopen the file and group in the file.
    file <- openFile fileName [ReadWrite] Nothing
    grp  <- openGroup file (BS.pack "Data") Nothing
    
    -- Access "Compressed_Data" dataset in the group.
    dataset <- openDataset grp (BS.pack "Compressed_Data") Nothing
--     if( dataset < 0) printf(" Dataset 'Compressed-Data' is not found. \n");
    putStrLn "\"/Data/Compressed_Data\" dataset is open"
    
    -- Close the dataset.
    closeDataset dataset
    
    -- Create hard link to the Data group.
    createHardLink file (BS.pack "Data") file (BS.pack "Data_new") Nothing Nothing
--     status = H5Lcreate_hard(file, "Data", H5L_SAME_LOC, "Data_new", H5P_DEFAULT, H5P_DEFAULT);
    
    -- We can access "Compressed_Data" dataset using created
    -- hard link "Data_new".
    dataset <- openDataset file (BS.pack "/Data_new/Compressed_Data") Nothing
--     if( dataset < 0) printf(" Dataset is not found. \n");
    putStrLn "\"/Data_new/Compressed_Data\" dataset is open"
    
    closeDataset dataset
    
    -- Use iterator to see the names of the objects in the root group.
    iterateLinks file ByName Increasing Nothing file_info
    
    -- Unlink  name "Data" and use iterator to see the names
    -- of the objects in the file root direvtory.
    
    deleteLink file (BS.pack "Data") Nothing
--     if(H5Ldelete(file, "Data", H5P_DEFAULT) < 0)
--       printf(" H5Ldelete failed \n");
--     else
    putStrLn "\"Data\" is unlinked"
    
    iterateLinks file ByName Increasing Nothing file_info
    
    -- Use iterator to see the names of the objects in the group
    -- /Data_new.
    iterateLinksByName grp (BS.pack "/Data_new") ByName Increasing Nothing Nothing group_info
    
    -- Close the file.
    
    closeGroup grp
    closeFile file
    
    return ()
-- 
-- Operator function.
file_info grp name linkInfo = do
    putStrLn ("\nName : " ++ BS.unpack name)
    return 0

-- Operator function.
group_info grp name linkInfo = do
    -- Open the datasets using their names.
    did <- openDataset grp name Nothing
    
    -- Display dataset name.
    putStrLn ("Name : " ++ BS.unpack name)
    
    -- Display dataset information.
    tid <- getDatasetType did
    pid <- getDatasetCreatePList did
    
    -- Check if dataset is chunked.
    layout <- getLayout pid
    if layout == Chunked
        then do
            -- get chunking information: rank and dimensions.
            chunk_dims_out <- getChunk pid
            putStrLn ("chunk dims out: " ++ show chunk_dims_out)
        else do
            t_class <- getTypeClass tid
            putStrLn ("Datatype is " ++ show t_class)
    
    closeDataset did
    closePropertyList pid
    closeTypeID tid
    
    return 0
