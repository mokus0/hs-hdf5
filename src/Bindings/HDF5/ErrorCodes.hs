{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Bindings.HDF5.ErrorCodes 
    ( MajorErrCode(..), majorErrorCode, majorErrorFromCode
    , MinorErrCode(..), minorErrorCode, minorErrorFromCode

    ) where

import Bindings.HDF5.Raw.H5E
import Bindings.HDF5.Raw.H5I
import Data.Typeable (Typeable)
import Foreign

data MajorErrCode
    -- |Dataset
    = Dataset
    -- |Function entry/exit
    | Func
    -- |Data storage
    | Storage
    -- |File accessability
    | File
    -- |Shared Object Header Messages
    | SOHM
    -- |Symbol table
    | Sym
    -- |Virtual File Layer
    | VFL
    -- |Internal error (too specific to document in detail)
    | Internal
    -- |B-Tree node
    | BTree
    -- |References
    | Reference
    -- |Dataspace
    | Dataspace
    -- |Resource unavailable
    | Resource
    -- |Property lists
    | PList
    -- |Links
    | Link
    -- |Datatype
    | Datatype
    -- |Reference Counted Strings
    | RS
    -- |Heap
    | Heap
    -- |Object header
    | OHDR
    -- |Object atom
    | Atom
    -- |Attribute
    | Attr
    -- |Low-level I/O
    | IO
    -- |Skip Lists
    | SList
    -- |External file list
    | EFL
    -- |Ternary Search Trees
    | TST
    -- |Invalid arguments to routine
    | Args
    -- |Error API
    | Error
    -- |Data filters
    | PLine
    -- |Free Space Manager
    | FSpace
    -- |Object cache
    | Cache
    
    -- |Unrecognized major error code
    | UnknownMajor HId_t
    deriving (Eq, Ord, Show, Typeable)

rawMajorErrCodesInv = [(a,b) | (b,a) <- rawMajorErrCodes]
rawMajorErrCodes =
    [ (Nothing,         h5e_NONE_MAJOR)
    , (Just Dataset,    h5e_DATASET)
    , (Just Func,       h5e_FUNC)
    , (Just Storage,    h5e_STORAGE)
    , (Just File,       h5e_FILE)
    , (Just SOHM,       h5e_SOHM)
    , (Just Sym,        h5e_SYM)
    , (Just VFL,        h5e_VFL)
    , (Just Internal,   h5e_INTERNAL)
    , (Just BTree,      h5e_BTREE)
    , (Just Reference,  h5e_REFERENCE)
    , (Just Dataspace,  h5e_DATASPACE)
    , (Just Resource,   h5e_RESOURCE)
    , (Just PList,      h5e_PLIST)
    , (Just Link,       h5e_LINK)
    , (Just Datatype,   h5e_DATATYPE)
    , (Just RS,         h5e_RS)
    , (Just Heap,       h5e_HEAP)
    , (Just OHDR,       h5e_OHDR)
    , (Just Atom,       h5e_ATOM)
    , (Just Attr,       h5e_ATTR)
    , (Just IO,         h5e_IO)
    , (Just SList,      h5e_SLIST)
    , (Just EFL,        h5e_EFL)
    , (Just TST,        h5e_TST)
    , (Just Args,       h5e_ARGS)
    , (Just Error,      h5e_ERROR)
    , (Just PLine,      h5e_PLINE)
    , (Just FSpace,     h5e_FSPACE)
    , (Just Cache,      h5e_CACHE)
    ]

majorErrorCode :: Maybe MajorErrCode -> Maybe HId_t
majorErrorCode (Just (UnknownMajor code)) = Just code
majorErrorCode err = lookup err rawMajorErrCodes

majorErrorFromCode :: HId_t -> Maybe MajorErrCode
majorErrorFromCode code = case lookup code rawMajorErrCodesInv of
    Nothing  -> Just (UnknownMajor code)
    Just err -> err

instance Storable (Maybe MajorErrCode) where
    sizeOf    _ = sizeOf    (undefined :: HId_t)
    alignment _ = alignment (undefined :: HId_t)
    
    peek = fmap majorErrorFromCode . peek . castPtr
    poke p err = case majorErrorCode err of
        Nothing -> fail ("Unknown major error code: " ++ show err)
        Just code -> poke (castPtr p) code

data MinorErrCode
    = SeekError
    -- |Read failed
    | ReadError
    -- |Write failed
    | WriteError
    -- |Close failed
    | CloseError
    -- |Address overflowed
    | Overflow
    -- |File control (fcntl) failed
    | FCNTL
    -- |No space available for allocation
    | NoSpace
    -- |Can't allocate space
    | CantAlloc
    -- |Unable to copy object
    | CantCopy
    -- |Unable to free object
    | CantFree
    -- |Object already exists
    | AlreadyExists
    -- |Unable to lock object
    | CantLock
    -- |Unable to unlock object
    | CantUnlock
    -- |Unable to garbage collect
    | CantGC
    -- |Unable to compute size
    | CantGetSize
    -- |Object is already open
    | ObjOpen
    -- |Can't restore condition
    | CantRestore
    -- |Can't compute value
    | CantCompute
    -- |Can't extend heap's space
    | CantExtend
    -- |Can't attach object
    | CantAttach
    -- |Can't update object
    | CantUpdate
    -- |Can't operate on object
    | CantOperate
    -- |Unable to initialize object
    | CantInit
    -- |Object already initialized
    | AlreadyInit
    -- |Unable to release object
    | CantRelease
    -- |Can't get value
    | CantGet
    -- |Can't set value
    | CantSet
    -- |Duplicate class name in parent class
    | DupClass
    -- |Can't merge objects
    | CantMerge
    -- |Can't revive object
    | CantRevive
    -- |Can't shrink container
    | CantShrink
    -- |Bad object header link count
    | LinkCount
    -- |Wrong version number
    | Version
    -- |Alignment error
    | Alignment
    -- |Unrecognized message
    | BadMesg
    -- |Can't delete message
    | CantDelete
    -- |Iteration failed
    | BadIter
    -- |Can't pack messages
    | CantPack
    -- |Can't reset object
    | CantReset
    -- |Unable to rename object
    | CantRename
    -- |System error message
    | SysErrStr
    -- |Requested filter is not available
    | NoFilter
    -- |Callback failed
    | Callback
    -- |Error from filter 'can apply' callback
    | CanApply
    -- |Error from filter 'set local' callback
    | SetLocal
    -- |Filter present but encoding disabled
    | NoEncoder
    -- |Filter operation failed
    | CantFilter
    -- |Can't open object
    | CantOpenObj
    -- |Can't close object
    | CantCloseObj
    -- |Name component is too long
    | CompLen
    -- |Problem with path to object
    | Path
    -- |File already exists
    | FileExists
    -- |File already open
    | FileOpen
    -- |Unable to create file
    | CantCreate
    -- |Unable to open file
    | CantOpenFile
    -- |Unable to close file
    | CantCloseFile
    -- |Not an HDF5 file
    | NotHDF5
    -- |Bad file ID accessed
    | BadFile
    -- |File has been truncated
    | Truncated
    -- |File mount error
    | Mount
    -- |Unable to find atom information (already closed?)
    | BadAtom
    -- |Unable to find ID group information
    | BadGroup
    -- |Unable to register new atom
    | CantRegister
    -- |Unable to increment reference count
    | CantInc
    -- |Unable to decrement reference count
    | CantDec
    -- |Out of IDs for group
    | NoIds
    -- |Unable to flush data from cache
    | CantFlush
    -- |Unable to serialize data from cache
    | CantSerialize
    -- |Unable to load metadata into cache
    | CantLoad
    -- |Protected metadata error
    | Protect
    -- |Metadata not currently cached
    | NotCached
    -- |Internal error detected
    | System
    -- |Unable to insert metadata into cache
    | CantIns
    -- |Unable to protect metadata
    | CantProtect
    -- |Unable to unprotect metadata
    | CanTUnprotect
    -- |Unable to pin cache entry
    | CantPin
    -- |Unable to un-pin cache entry
    | CantUnpin
    -- |Unable to mark a pinned entry as dirty
    | CantMarkDirty
    -- |Unable to mark metadata as dirty
    | CantDirty
    -- |Unable to expunge a metadata cache entry
    | CantExpunge
    -- |Unable to resize a metadata cache entry
    | CantResize
    -- |Link traversal failure
    | Traverse
    -- |Too many soft links in path
    | NLinks
    -- |Link class not registered
    | NotRegistered
    -- |Can't move object
    | CantMove
    -- |Can't sort objects
    | CantSort
    -- |Some MPI function failed
    | MPI
    -- |MPI Error String
    | MPIErrStr
    -- |Can't receive data
    | CantRecv
    -- |Can't clip hyperslab region
    | CantClip
    -- |Can't count elements
    | CantCount
    -- |Can't select hyperslab
    | CantSelect
    -- |Can't move to next iterator location
    | CantNext
    -- |Invalid selection
    | BadSelect
    -- |Can't compare objects
    | CantCompare
    -- |Information is uinitialized
    | Uninitialized
    -- |Feature is unsupported
    | Unsupported
    -- |Inappropriate type
    | BadType
    -- |Out of range
    | BadRange
    -- |Bad value
    | BadValue
    -- |Object not found
    | NotFound
    -- |Object already exists
    | Exists
    -- |Unable to encode value
    | CantEncode
    -- |Unable to decode value
    | CantDecode
    -- |Unable to split node
    | CantSplit
    -- |Unable to redistribute records
    | CantRedistribute
    -- |Unable to swap records
    | CantSwap
    -- |Unable to insert object
    | CantInsert
    -- |Unable to list node
    | CantList
    -- |Unable to modify record
    | CantModify
    -- |Unable to remove object
    | CantRemove
    -- |Can't convert datatypes
    | CantConvert
    -- |Bad size for object
    | BadSize

    -- |Unrecognized minor error code
    | UnknownMinor HId_t
    deriving (Eq, Ord, Show, Typeable)

rawMinorErrCodesInv = [(a,b) | (b,a) <- rawMinorErrCodes]
rawMinorErrCodes =
    [ (Nothing,                 h5e_NONE_MINOR)
    , (Just SeekError,          h5e_SEEKERROR)
    , (Just ReadError,          h5e_READERROR)
    , (Just WriteError,         h5e_WRITEERROR)
    , (Just CloseError,         h5e_CLOSEERROR)
    , (Just Overflow,           h5e_OVERFLOW)
    , (Just FCNTL,              h5e_FCNTL)
    , (Just NoSpace,            h5e_NOSPACE)
    , (Just CantAlloc,          h5e_CANTALLOC)
    , (Just CantCopy,           h5e_CANTCOPY)
    , (Just CantFree,           h5e_CANTFREE)
    , (Just AlreadyExists,      h5e_ALREADYEXISTS)
    , (Just CantLock,           h5e_CANTLOCK)
    , (Just CantUnlock,         h5e_CANTUNLOCK)
    , (Just CantGC,             h5e_CANTGC)
    , (Just CantGetSize,        h5e_CANTGETSIZE)
    , (Just ObjOpen,            h5e_OBJOPEN)
    , (Just CantRestore,        h5e_CANTRESTORE)
    , (Just CantCompute,        h5e_CANTCOMPUTE)
    , (Just CantExtend,         h5e_CANTEXTEND)
    , (Just CantAttach,         h5e_CANTATTACH)
    , (Just CantUpdate,         h5e_CANTUPDATE)
    , (Just CantOperate,        h5e_CANTOPERATE)
    , (Just CantInit,           h5e_CANTINIT)
    , (Just AlreadyInit,        h5e_ALREADYINIT)
    , (Just CantRelease,        h5e_CANTRELEASE)
    , (Just CantGet,            h5e_CANTGET)
    , (Just CantSet,            h5e_CANTSET)
    , (Just DupClass,           h5e_DUPCLASS)
    , (Just CantMerge,          h5e_CANTMERGE)
    , (Just CantRevive,         h5e_CANTREVIVE)
    , (Just CantShrink,         h5e_CANTSHRINK)
    , (Just LinkCount,          h5e_LINKCOUNT)
    , (Just Version,            h5e_VERSION)
    , (Just Alignment,          h5e_ALIGNMENT)
    , (Just BadMesg,            h5e_BADMESG)
    , (Just CantDelete,         h5e_CANTDELETE)
    , (Just BadIter,            h5e_BADITER)
    , (Just CantPack,           h5e_CANTPACK)
    , (Just CantReset,          h5e_CANTRESET)
    , (Just CantRename,         h5e_CANTRENAME)
    , (Just SysErrStr,          h5e_SYSERRSTR)
    , (Just NoFilter,           h5e_NOFILTER)
    , (Just Callback,           h5e_CALLBACK)
    , (Just CanApply,           h5e_CANAPPLY)
    , (Just SetLocal,           h5e_SETLOCAL)
    , (Just NoEncoder,          h5e_NOENCODER)
    , (Just CantFilter,         h5e_CANTFILTER)
    , (Just CantOpenObj,        h5e_CANTOPENOBJ)
    , (Just CantCloseObj,       h5e_CANTCLOSEOBJ)
    , (Just CompLen,            h5e_COMPLEN)
    , (Just Path,               h5e_PATH)
    , (Just FileExists,         h5e_FILEEXISTS)
    , (Just FileOpen,           h5e_FILEOPEN)
    , (Just CantCreate,         h5e_CANTCREATE)
    , (Just CantOpenFile,       h5e_CANTOPENFILE)
    , (Just CantCloseFile,      h5e_CANTCLOSEFILE)
    , (Just NotHDF5,            h5e_NOTHDF5)
    , (Just BadFile,            h5e_BADFILE)
    , (Just Truncated,          h5e_TRUNCATED)
    , (Just Mount,              h5e_MOUNT)
    , (Just BadAtom,            h5e_BADATOM)
    , (Just BadGroup,           h5e_BADGROUP)
    , (Just CantRegister,       h5e_CANTREGISTER)
    , (Just CantInc,            h5e_CANTINC)
    , (Just CantDec,            h5e_CANTDEC)
    , (Just NoIds,              h5e_NOIDS)
    , (Just CantFlush,          h5e_CANTFLUSH)
    , (Just CantSerialize,      h5e_CANTSERIALIZE)
    , (Just CantLoad,           h5e_CANTLOAD)
    , (Just Protect,            h5e_PROTECT)
    , (Just NotCached,          h5e_NOTCACHED)
    , (Just System,             h5e_SYSTEM)
    , (Just CantIns,            h5e_CANTINS)
    , (Just CantProtect,        h5e_CANTPROTECT)
    , (Just CanTUnprotect,      h5e_CANTUNPROTECT)
    , (Just CantPin,            h5e_CANTPIN)
    , (Just CantUnpin,          h5e_CANTUNPIN)
    , (Just CantMarkDirty,      h5e_CANTMARKDIRTY)
    , (Just CantDirty,          h5e_CANTDIRTY)
    , (Just CantExpunge,        h5e_CANTEXPUNGE)
    , (Just CantResize,         h5e_CANTRESIZE)
    , (Just Traverse,           h5e_TRAVERSE)
    , (Just NLinks,             h5e_NLINKS)
    , (Just NotRegistered,      h5e_NOTREGISTERED)
    , (Just CantMove,           h5e_CANTMOVE)
    , (Just CantSort,           h5e_CANTSORT)
    , (Just MPI,                h5e_MPI)
    , (Just MPIErrStr,          h5e_MPIERRSTR)
    , (Just CantRecv,           h5e_CANTRECV)
    , (Just CantClip,           h5e_CANTCLIP)
    , (Just CantCount,          h5e_CANTCOUNT)
    , (Just CantSelect,         h5e_CANTSELECT)
    , (Just CantNext,           h5e_CANTNEXT)
    , (Just BadSelect,          h5e_BADSELECT)
    , (Just CantCompare,        h5e_CANTCOMPARE)
    , (Just Uninitialized,      h5e_UNINITIALIZED)
    , (Just Unsupported,        h5e_UNSUPPORTED)
    , (Just BadType,            h5e_BADTYPE)
    , (Just BadRange,           h5e_BADRANGE)
    , (Just BadValue,           h5e_BADVALUE)
    , (Just NotFound,           h5e_NOTFOUND)
    , (Just Exists,             h5e_EXISTS)
    , (Just CantEncode,         h5e_CANTENCODE)
    , (Just CantDecode,         h5e_CANTDECODE)
    , (Just CantSplit,          h5e_CANTSPLIT)
    , (Just CantRedistribute,   h5e_CANTREDISTRIBUTE)
    , (Just CantSwap,           h5e_CANTSWAP)
    , (Just CantInsert,         h5e_CANTINSERT)
    , (Just CantList,           h5e_CANTLIST)
    , (Just CantModify,         h5e_CANTMODIFY)
    , (Just CantRemove,         h5e_CANTREMOVE)
    , (Just CantConvert,        h5e_CANTCONVERT)
    , (Just BadSize,            h5e_BADSIZE)
    ]

minorErrorCode :: Maybe MinorErrCode -> Maybe HId_t
minorErrorCode (Just (UnknownMinor code)) = Just code
minorErrorCode err = lookup err rawMinorErrCodes

minorErrorFromCode :: HId_t -> Maybe MinorErrCode
minorErrorFromCode code = case lookup code rawMinorErrCodesInv of
    Nothing  -> Just (UnknownMinor code)
    Just err -> err

instance Storable (Maybe MinorErrCode) where
    sizeOf    _ = sizeOf    (undefined :: HId_t)
    alignment _ = alignment (undefined :: HId_t)
    
    peek = fmap minorErrorFromCode . peek . castPtr
    poke p err = case minorErrorCode err of
        Nothing -> fail ("Unknown minor error code: " ++ show err)
        Just code -> poke (castPtr p) code
