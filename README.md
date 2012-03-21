HDF5 for Haskell
=================

This is a low-level but typesafe Haskell interface to the HDF5 library.  No pointers necessary.

In theory, it should not be possible to write code that corrupts or leaks memory using this package.  That's the goal anyway.  It has probably not been achieved.  If you find any interface that doesn't live up to that goal, please open a ticket in the github issue tracker or email the maintainer.

Generally speaking, "type"-related exceptions should not occur either as long as the client code isn't monkeying about with unchecked hid_t casts, etc.

This interface does not aim to cover all of HDF5, at least not right now.  If there's any missing functionality you need, let the maintainer know and we'll be happy to accept patches or (if we have the time) implement it.

Installation
-------------

First, you'll need to install the [low level bindings](https://github.com/mokus0/bindings-hdf5) - see that project's [README](https://github.com/mokus0/bindings-hdf5/blob/master/README.md) for instructions.  Once you've done that, this package can be installed using cabal:

    git clone https://github.com/mokus0/hs-hdf5.git
    cd hs-hdf5
    cabal install

Usage
------

Step 1:

    import Bindings.HDF5

Step 2:

Go nuts!  (You probably will want to keep [the HDF5 reference](http://www.hdfgroup.org/HDF5/doc/RM/RM_H5Front.html) handy)

    import Bindings.HDF5
    import Control.Monad
    import qualified Data.ByteString.UTF8 as BS
    import Data.Random
    import qualified Data.Vector.Storable as V
    import qualified Data.Vector.Storable.Mutable as M
    
    main = do
        -- first a silly "calculation", so we have something to write:
        densityVec <- M.replicate (10*10*10) (0.0 :: Float)
        replicateM_ 1000000 $ do
            x <- sample (normal 5 3) :: IO Float
            y <- sample (normal 5 3) :: IO Float
            z <- sample (normal 5 3) :: IO Float
            let -- location in the vector - writeDataset uses "row major" order.
                i = round x * 10 * 10
                  + round y * 10
                  + round z
            
            when (0 <= i && i < 10^3) $ do
                d <- M.read densityVec i
                M.write densityVec i (d + 1e-3)
        densityVec <- V.freeze densityVec
        
        -- now write it to a file:
        file <- createFile (BS.fromString "foo.h5") [Truncate] Nothing Nothing
        dataspace <- createSimpleDataspace [10,10,10]
        dataset <- createDataset file (BS.fromString "density") nativeFloat dataspace
                        Nothing Nothing Nothing
        writeDataset dataset Nothing Nothing Nothing densityVec
        
        closeDataset dataset
        closeDataspace dataspace
        closeFile file

For more examples, take a look at [the examples directory](https://github.com/mokus0/hs-hdf5/tree/master/examples).
