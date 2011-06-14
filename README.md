This is a low-level but typesafe Haskell interface to the HDF5 library.  No pointers necessary.

In theory, it should not be possible to write HDF5 client code that corrupts or leaks memory.  That's the goal anyway.  It has probably not been achieved.  If you find any interface that doesn't live up to that goal, please open a ticket in the github issue tracker or email the maintainer.

Generally speaking, "type"-related exceptions should not occur either as long as the client code isn't monkeying about with unchecked hid_t casts, etc.

This interface does not aim to cover all of HDF5, at least not right now.  If there's any missing functionality you need, let the maintainer know and we'll be happy to accept patches or (if we have the time) implement it.