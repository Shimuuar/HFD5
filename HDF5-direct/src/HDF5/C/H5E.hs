{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
-- API for error handling
module HDF5.C.H5E
  ( -- * Constants and types
    h5e_DEFAULT
  , H5EDirection(..)
  , pattern H5E_WALK_UPWARD
  , pattern H5E_WALK_DOWNWARD
  , H5EType(..)
  , pattern H5E_MAJOR
  , pattern H5E_MINOR
    -- ** struct H5EError2
  , H5EError
  , h5e_error_cls_id
  , h5e_error_maj_num
  , h5e_error_min_num
  , h5e_error_line
  , h5e_error_func_name
  , h5e_error_file_name
  , h5e_error_desc
    -- ** Callback types
  , H5EAuto
  , H5EWalk
  , makeWalker
    -- * Functions
  , h5e_close_msg
  , h5e_close_stack
  , h5e_create_stack
  , h5e_get_current_stack
  , h5e_get_auto
  , h5e_set_auto
  , h5e_walk
  , h5e_get_msg
    -- * Error codes
    -- ** Major
  , c_H5E_ARGS
  , c_H5E_ATTR
  , c_H5E_BTREE
  , c_H5E_CACHE
  , c_H5E_CONTEXT
  , c_H5E_DATASET
  , c_H5E_DATASPACE
  , c_H5E_DATATYPE
  , c_H5E_EARRAY
  , c_H5E_EFL
  , c_H5E_ERROR
  , c_H5E_EVENTSET
  , c_H5E_FARRAY
  , c_H5E_FILE
  , c_H5E_FSPACE
  , c_H5E_FUNC
  , c_H5E_HEAP
  , c_H5E_ID
  , c_H5E_INTERNAL
  , c_H5E_IO
  , c_H5E_LIB
  , c_H5E_LINK
  , c_H5E_MAP
  , c_H5E_NONE_MAJOR
  , c_H5E_OHDR
  , c_H5E_PAGEBUF
  , c_H5E_PLINE
  , c_H5E_PLIST
  , c_H5E_PLUGIN
  , c_H5E_REFERENCE
  , c_H5E_RESOURCE
  , c_H5E_RS
  , c_H5E_SLIST
  , c_H5E_SOHM
  , c_H5E_STORAGE
  , c_H5E_SYM
  , c_H5E_TST
  , c_H5E_VFL
  , c_H5E_VOL
    -- ** Minor
  , c_H5E_UNINITIALIZED
  , c_H5E_UNSUPPORTED
  , c_H5E_BADTYPE
  , c_H5E_BADRANGE
  , c_H5E_BADVALUE
  , c_H5E_NOSPACE
  , c_H5E_CANTALLOC
  , c_H5E_CANTCOPY
  , c_H5E_CANTFREE
  , c_H5E_ALREADYEXISTS
  , c_H5E_CANTLOCK
  , c_H5E_CANTUNLOCK
  , c_H5E_CANTGC
  , c_H5E_CANTGETSIZE
  , c_H5E_OBJOPEN
  , c_H5E_FILEEXISTS
  , c_H5E_FILEOPEN
  , c_H5E_CANTCREATE
  , c_H5E_CANTOPENFILE
  , c_H5E_CANTCLOSEFILE
  , c_H5E_NOTHDF5
  , c_H5E_BADFILE
  , c_H5E_TRUNCATED
  , c_H5E_MOUNT
  , c_H5E_UNMOUNT
  , c_H5E_CANTDELETEFILE
  , c_H5E_CANTLOCKFILE
  , c_H5E_CANTUNLOCKFILE
  , c_H5E_SEEKERROR
  , c_H5E_READERROR
  , c_H5E_WRITEERROR
  , c_H5E_CLOSEERROR
  , c_H5E_OVERFLOW
  , c_H5E_FCNTL
  , c_H5E_CANTINIT
  , c_H5E_ALREADYINIT
  , c_H5E_CANTRELEASE
  , c_H5E_BADID
  , c_H5E_BADGROUP
  , c_H5E_CANTREGISTER
  , c_H5E_CANTINC
  , c_H5E_CANTDEC
  , c_H5E_NOIDS
  , c_H5E_CANTFLUSH
  , c_H5E_CANTUNSERIALIZE
  , c_H5E_CANTSERIALIZE
  , c_H5E_CANTTAG
  , c_H5E_CANTLOAD
  , c_H5E_PROTECT
  , c_H5E_NOTCACHED
  , c_H5E_SYSTEM
  , c_H5E_CANTINS
  , c_H5E_CANTPROTECT
  , c_H5E_CANTUNPROTECT
  , c_H5E_CANTPIN
  , c_H5E_CANTUNPIN
  , c_H5E_CANTMARKDIRTY
  , c_H5E_CANTMARKCLEAN
  , c_H5E_CANTMARKUNSERIALIZED
  , c_H5E_CANTMARKSERIALIZED
  , c_H5E_CANTDIRTY
  , c_H5E_CANTCLEAN
  , c_H5E_CANTEXPUNGE
  , c_H5E_CANTRESIZE
  , c_H5E_CANTDEPEND
  , c_H5E_CANTUNDEPEND
  , c_H5E_CANTNOTIFY
  , c_H5E_LOGGING
  , c_H5E_CANTCORK
  , c_H5E_CANTUNCORK
  , c_H5E_NOTFOUND
  , c_H5E_EXISTS
  , c_H5E_CANTENCODE
  , c_H5E_CANTDECODE
  , c_H5E_CANTSPLIT
  , c_H5E_CANTREDISTRIBUTE
  , c_H5E_CANTSWAP
  , c_H5E_CANTINSERT
  , c_H5E_CANTLIST
  , c_H5E_CANTMODIFY
  , c_H5E_CANTREMOVE
  , c_H5E_CANTFIND
  , c_H5E_LINKCOUNT
  , c_H5E_VERSION
  , c_H5E_ALIGNMENT
  , c_H5E_BADMESG
  , c_H5E_CANTDELETE
  , c_H5E_BADITER
  , c_H5E_CANTPACK
  , c_H5E_CANTRESET
  , c_H5E_CANTRENAME
  , c_H5E_CANTOPENOBJ
  , c_H5E_CANTCLOSEOBJ
  , c_H5E_COMPLEN
  , c_H5E_PATH
  , c_H5E_CANTCONVERT
  , c_H5E_BADSIZE
  , c_H5E_CANTCLIP
  , c_H5E_CANTCOUNT
  , c_H5E_CANTSELECT
  , c_H5E_CANTNEXT
  , c_H5E_BADSELECT
  , c_H5E_CANTCOMPARE
  , c_H5E_INCONSISTENTSTATE
  , c_H5E_CANTAPPEND
  , c_H5E_CANTGET
  , c_H5E_CANTSET
  , c_H5E_DUPCLASS
  , c_H5E_SETDISALLOWED
  , c_H5E_TRAVERSE
  , c_H5E_NLINKS
  , c_H5E_NOTREGISTERED
  , c_H5E_CANTMOVE
  , c_H5E_CANTSORT
  , c_H5E_MPI
  , c_H5E_MPIERRSTR
  , c_H5E_CANTRECV
  , c_H5E_CANTGATHER
  , c_H5E_NO_INDEPENDENT
  , c_H5E_CANTRESTORE
  , c_H5E_CANTCOMPUTE
  , c_H5E_CANTEXTEND
  , c_H5E_CANTATTACH
  , c_H5E_CANTUPDATE
  , c_H5E_CANTOPERATE
  , c_H5E_CANTMERGE
  , c_H5E_CANTREVIVE
  , c_H5E_CANTSHRINK
  , c_H5E_NOFILTER
  , c_H5E_CALLBACK
  , c_H5E_CANAPPLY
  , c_H5E_SETLOCAL
  , c_H5E_NOENCODER
  , c_H5E_CANTFILTER
  , c_H5E_SYSERRSTR
  , c_H5E_OPENERROR
  , c_H5E_CANTPUT
  , c_H5E_CANTWAIT
  , c_H5E_CANTCANCEL
  , c_H5E_NONE_MINOR
  ) where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Default value to use with functions in this module
foreign import capi "hdf5.h value H5E_DEFAULT" h5e_DEFAULT :: HID


-- | Direction in which error stack is walked
newtype H5EDirection = H5EDirection CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5E_WALK_DOWNWARD" h5e_WALK_DOWNWARD :: H5EDirection
foreign import capi "hdf5.h value H5E_WALK_UPWARD"   h5e_WALK_UPWARD   :: H5EDirection

pattern H5E_WALK_UPWARD :: H5EDirection
pattern H5E_WALK_UPWARD <- ((==h5e_WALK_UPWARD) -> True) where H5E_WALK_UPWARD = h5e_WALK_UPWARD

pattern H5E_WALK_DOWNWARD :: H5EDirection
pattern H5E_WALK_DOWNWARD <- ((==h5e_WALK_DOWNWARD) -> True) where H5E_WALK_DOWNWARD = h5e_WALK_DOWNWARD


newtype H5EType = H5EType CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5E_MAJOR" h5e_MAJOR :: H5EType
foreign import capi "hdf5.h value H5E_MINOR" h5e_MINOR :: H5EType

pattern H5E_MAJOR :: H5EType
pattern H5E_MAJOR <- ((==h5e_MAJOR) -> True) where H5E_MAJOR = h5e_MAJOR

pattern H5E_MINOR :: H5EType
pattern H5E_MINOR <- ((==h5e_MINOR) -> True) where H5E_MINOR = h5e_MINOR


-- | Tag for struct @H5Eerror2_t@
--
-- > hid_t       cls_id
-- > hid_t       maj_num
-- > hid_t       min_num
-- > unsigned    line
-- > const char *func_name
-- > const char *file_name
-- > const char *desc
data H5EError

foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_cls_id"    off_cls_id    :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_maj_num"   off_maj_num   :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_min_num"   off_min_num   :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_line"      off_line      :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_func_name" off_func_name :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_file_name" off_file_name :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_desc"      off_desc      :: CSize

h5e_error_cls_id :: Ptr H5EError -> Ptr HID
h5e_error_cls_id p = castPtr $ plusPtr p (fromIntegral off_cls_id)

h5e_error_maj_num :: Ptr H5EError -> Ptr HID
h5e_error_maj_num p = castPtr $ plusPtr p (fromIntegral off_maj_num)

h5e_error_min_num :: Ptr H5EError -> Ptr HID
h5e_error_min_num p = castPtr $ plusPtr p (fromIntegral off_min_num)

h5e_error_line :: Ptr H5EError -> Ptr CUInt
h5e_error_line p = castPtr $ plusPtr p (fromIntegral off_line)

h5e_error_func_name :: Ptr H5EError -> Ptr CString
h5e_error_func_name p = castPtr $ plusPtr p (fromIntegral off_func_name)

h5e_error_file_name :: Ptr H5EError -> Ptr CString
h5e_error_file_name p = castPtr $ plusPtr p (fromIntegral off_file_name)

h5e_error_desc :: Ptr H5EError -> Ptr CString
h5e_error_desc p = castPtr $ plusPtr p (fromIntegral off_desc)

----------------------------------------------------------------
-- Error codes
----------------------------------------------------------------

-- HDF5 over time adds more error. We define missing ones as
-- H5I_INVALID_HID in compat header. This should not cause problems
-- since we only use error codes to extract data from HDF5
-- error. H5I_INVALID_HID should not appear there.


foreign import capi "hdf5-hs.h value H5E_ARGS"       c_H5E_ARGS       :: HID
foreign import capi "hdf5-hs.h value H5E_ATTR"       c_H5E_ATTR       :: HID
foreign import capi "hdf5-hs.h value H5E_BTREE"      c_H5E_BTREE      :: HID
foreign import capi "hdf5-hs.h value H5E_CACHE"      c_H5E_CACHE      :: HID
foreign import capi "hdf5-hs.h value H5E_CONTEXT"    c_H5E_CONTEXT    :: HID
foreign import capi "hdf5-hs.h value H5E_DATASET"    c_H5E_DATASET    :: HID
foreign import capi "hdf5-hs.h value H5E_DATASPACE"  c_H5E_DATASPACE  :: HID
foreign import capi "hdf5-hs.h value H5E_DATATYPE"   c_H5E_DATATYPE   :: HID
foreign import capi "hdf5-hs.h value H5E_EARRAY"     c_H5E_EARRAY     :: HID
foreign import capi "hdf5-hs.h value H5E_EFL"        c_H5E_EFL        :: HID
foreign import capi "hdf5-hs.h value H5E_ERROR"      c_H5E_ERROR      :: HID
foreign import capi "hdf5-hs.h value H5E_EVENTSET"   c_H5E_EVENTSET   :: HID
foreign import capi "hdf5-hs.h value H5E_FARRAY"     c_H5E_FARRAY     :: HID
foreign import capi "hdf5-hs.h value H5E_FILE"       c_H5E_FILE       :: HID
foreign import capi "hdf5-hs.h value H5E_FSPACE"     c_H5E_FSPACE     :: HID
foreign import capi "hdf5-hs.h value H5E_FUNC"       c_H5E_FUNC       :: HID
foreign import capi "hdf5-hs.h value H5E_HEAP"       c_H5E_HEAP       :: HID
foreign import capi "hdf5-hs.h value H5E_ID"         c_H5E_ID         :: HID
foreign import capi "hdf5-hs.h value H5E_INTERNAL"   c_H5E_INTERNAL   :: HID
foreign import capi "hdf5-hs.h value H5E_IO"         c_H5E_IO         :: HID
foreign import capi "hdf5-hs.h value H5E_LIB"        c_H5E_LIB        :: HID
foreign import capi "hdf5-hs.h value H5E_LINK"       c_H5E_LINK       :: HID
foreign import capi "hdf5-hs.h value H5E_MAP"        c_H5E_MAP        :: HID
foreign import capi "hdf5-hs.h value H5E_NONE_MAJOR" c_H5E_NONE_MAJOR :: HID
foreign import capi "hdf5-hs.h value H5E_OHDR"       c_H5E_OHDR       :: HID
foreign import capi "hdf5-hs.h value H5E_PAGEBUF"    c_H5E_PAGEBUF    :: HID
foreign import capi "hdf5-hs.h value H5E_PLINE"      c_H5E_PLINE      :: HID
foreign import capi "hdf5-hs.h value H5E_PLIST"      c_H5E_PLIST      :: HID
foreign import capi "hdf5-hs.h value H5E_PLUGIN"     c_H5E_PLUGIN     :: HID
foreign import capi "hdf5-hs.h value H5E_REFERENCE"  c_H5E_REFERENCE  :: HID
foreign import capi "hdf5-hs.h value H5E_RESOURCE"   c_H5E_RESOURCE   :: HID
foreign import capi "hdf5-hs.h value H5E_RS"         c_H5E_RS         :: HID
foreign import capi "hdf5-hs.h value H5E_SLIST"      c_H5E_SLIST      :: HID
foreign import capi "hdf5-hs.h value H5E_SOHM"       c_H5E_SOHM       :: HID
foreign import capi "hdf5-hs.h value H5E_STORAGE"    c_H5E_STORAGE    :: HID
foreign import capi "hdf5-hs.h value H5E_SYM"        c_H5E_SYM        :: HID
foreign import capi "hdf5-hs.h value H5E_TST"        c_H5E_TST        :: HID
foreign import capi "hdf5-hs.h value H5E_VFL"        c_H5E_VFL        :: HID
foreign import capi "hdf5-hs.h value H5E_VOL"        c_H5E_VOL        :: HID


foreign import capi "hdf5-hs.h value H5E_UNINITIALIZED"        c_H5E_UNINITIALIZED        :: HID
foreign import capi "hdf5-hs.h value H5E_UNSUPPORTED"          c_H5E_UNSUPPORTED          :: HID
foreign import capi "hdf5-hs.h value H5E_BADTYPE"              c_H5E_BADTYPE              :: HID
foreign import capi "hdf5-hs.h value H5E_BADRANGE"             c_H5E_BADRANGE             :: HID
foreign import capi "hdf5-hs.h value H5E_BADVALUE"             c_H5E_BADVALUE             :: HID
foreign import capi "hdf5-hs.h value H5E_NOSPACE"              c_H5E_NOSPACE              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTALLOC"            c_H5E_CANTALLOC            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCOPY"             c_H5E_CANTCOPY             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTFREE"             c_H5E_CANTFREE             :: HID
foreign import capi "hdf5-hs.h value H5E_ALREADYEXISTS"        c_H5E_ALREADYEXISTS        :: HID
foreign import capi "hdf5-hs.h value H5E_CANTLOCK"             c_H5E_CANTLOCK             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTUNLOCK"           c_H5E_CANTUNLOCK           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTGC"               c_H5E_CANTGC               :: HID
foreign import capi "hdf5-hs.h value H5E_CANTGETSIZE"          c_H5E_CANTGETSIZE          :: HID
foreign import capi "hdf5-hs.h value H5E_OBJOPEN"              c_H5E_OBJOPEN              :: HID
foreign import capi "hdf5-hs.h value H5E_FILEEXISTS"           c_H5E_FILEEXISTS           :: HID
foreign import capi "hdf5-hs.h value H5E_FILEOPEN"             c_H5E_FILEOPEN             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCREATE"           c_H5E_CANTCREATE           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTOPENFILE"         c_H5E_CANTOPENFILE         :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCLOSEFILE"        c_H5E_CANTCLOSEFILE        :: HID
foreign import capi "hdf5-hs.h value H5E_NOTHDF5"              c_H5E_NOTHDF5              :: HID
foreign import capi "hdf5-hs.h value H5E_BADFILE"              c_H5E_BADFILE              :: HID
foreign import capi "hdf5-hs.h value H5E_TRUNCATED"            c_H5E_TRUNCATED            :: HID
foreign import capi "hdf5-hs.h value H5E_MOUNT"                c_H5E_MOUNT                :: HID
foreign import capi "hdf5-hs.h value H5E_UNMOUNT"              c_H5E_UNMOUNT              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTDELETEFILE"       c_H5E_CANTDELETEFILE       :: HID
foreign import capi "hdf5-hs.h value H5E_CANTLOCKFILE"         c_H5E_CANTLOCKFILE         :: HID
foreign import capi "hdf5-hs.h value H5E_CANTUNLOCKFILE"       c_H5E_CANTUNLOCKFILE       :: HID
foreign import capi "hdf5-hs.h value H5E_SEEKERROR"            c_H5E_SEEKERROR            :: HID
foreign import capi "hdf5-hs.h value H5E_READERROR"            c_H5E_READERROR            :: HID
foreign import capi "hdf5-hs.h value H5E_WRITEERROR"           c_H5E_WRITEERROR           :: HID
foreign import capi "hdf5-hs.h value H5E_CLOSEERROR"           c_H5E_CLOSEERROR           :: HID
foreign import capi "hdf5-hs.h value H5E_OVERFLOW"             c_H5E_OVERFLOW             :: HID
foreign import capi "hdf5-hs.h value H5E_FCNTL"                c_H5E_FCNTL                :: HID
foreign import capi "hdf5-hs.h value H5E_CANTINIT"             c_H5E_CANTINIT             :: HID
foreign import capi "hdf5-hs.h value H5E_ALREADYINIT"          c_H5E_ALREADYINIT          :: HID
foreign import capi "hdf5-hs.h value H5E_CANTRELEASE"          c_H5E_CANTRELEASE          :: HID
foreign import capi "hdf5-hs.h value H5E_BADID"                c_H5E_BADID                :: HID
foreign import capi "hdf5-hs.h value H5E_BADGROUP"             c_H5E_BADGROUP             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTREGISTER"         c_H5E_CANTREGISTER         :: HID
foreign import capi "hdf5-hs.h value H5E_CANTINC"              c_H5E_CANTINC              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTDEC"              c_H5E_CANTDEC              :: HID
foreign import capi "hdf5-hs.h value H5E_NOIDS"                c_H5E_NOIDS                :: HID
foreign import capi "hdf5-hs.h value H5E_CANTFLUSH"            c_H5E_CANTFLUSH            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTUNSERIALIZE"      c_H5E_CANTUNSERIALIZE      :: HID
foreign import capi "hdf5-hs.h value H5E_CANTSERIALIZE"        c_H5E_CANTSERIALIZE        :: HID
foreign import capi "hdf5-hs.h value H5E_CANTTAG"              c_H5E_CANTTAG              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTLOAD"             c_H5E_CANTLOAD             :: HID
foreign import capi "hdf5-hs.h value H5E_PROTECT"              c_H5E_PROTECT              :: HID
foreign import capi "hdf5-hs.h value H5E_NOTCACHED"            c_H5E_NOTCACHED            :: HID
foreign import capi "hdf5-hs.h value H5E_SYSTEM"               c_H5E_SYSTEM               :: HID
foreign import capi "hdf5-hs.h value H5E_CANTINS"              c_H5E_CANTINS              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTPROTECT"          c_H5E_CANTPROTECT          :: HID
foreign import capi "hdf5-hs.h value H5E_CANTUNPROTECT"        c_H5E_CANTUNPROTECT        :: HID
foreign import capi "hdf5-hs.h value H5E_CANTPIN"              c_H5E_CANTPIN              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTUNPIN"            c_H5E_CANTUNPIN            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTMARKDIRTY"        c_H5E_CANTMARKDIRTY        :: HID
foreign import capi "hdf5-hs.h value H5E_CANTMARKCLEAN"        c_H5E_CANTMARKCLEAN        :: HID
foreign import capi "hdf5-hs.h value H5E_CANTMARKUNSERIALIZED" c_H5E_CANTMARKUNSERIALIZED :: HID
foreign import capi "hdf5-hs.h value H5E_CANTMARKSERIALIZED"   c_H5E_CANTMARKSERIALIZED   :: HID
foreign import capi "hdf5-hs.h value H5E_CANTDIRTY"            c_H5E_CANTDIRTY            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCLEAN"            c_H5E_CANTCLEAN            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTEXPUNGE"          c_H5E_CANTEXPUNGE          :: HID
foreign import capi "hdf5-hs.h value H5E_CANTRESIZE"           c_H5E_CANTRESIZE           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTDEPEND"           c_H5E_CANTDEPEND           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTUNDEPEND"         c_H5E_CANTUNDEPEND         :: HID
foreign import capi "hdf5-hs.h value H5E_CANTNOTIFY"           c_H5E_CANTNOTIFY           :: HID
foreign import capi "hdf5-hs.h value H5E_LOGGING"              c_H5E_LOGGING              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCORK"             c_H5E_CANTCORK             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTUNCORK"           c_H5E_CANTUNCORK           :: HID
foreign import capi "hdf5-hs.h value H5E_NOTFOUND"             c_H5E_NOTFOUND             :: HID
foreign import capi "hdf5-hs.h value H5E_EXISTS"               c_H5E_EXISTS               :: HID
foreign import capi "hdf5-hs.h value H5E_CANTENCODE"           c_H5E_CANTENCODE           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTDECODE"           c_H5E_CANTDECODE           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTSPLIT"            c_H5E_CANTSPLIT            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTREDISTRIBUTE"     c_H5E_CANTREDISTRIBUTE     :: HID
foreign import capi "hdf5-hs.h value H5E_CANTSWAP"             c_H5E_CANTSWAP             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTINSERT"           c_H5E_CANTINSERT           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTLIST"             c_H5E_CANTLIST             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTMODIFY"           c_H5E_CANTMODIFY           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTREMOVE"           c_H5E_CANTREMOVE           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTFIND"             c_H5E_CANTFIND             :: HID
foreign import capi "hdf5-hs.h value H5E_LINKCOUNT"            c_H5E_LINKCOUNT            :: HID
foreign import capi "hdf5-hs.h value H5E_VERSION"              c_H5E_VERSION              :: HID
foreign import capi "hdf5-hs.h value H5E_ALIGNMENT"            c_H5E_ALIGNMENT            :: HID
foreign import capi "hdf5-hs.h value H5E_BADMESG"              c_H5E_BADMESG              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTDELETE"           c_H5E_CANTDELETE           :: HID
foreign import capi "hdf5-hs.h value H5E_BADITER"              c_H5E_BADITER              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTPACK"             c_H5E_CANTPACK             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTRESET"            c_H5E_CANTRESET            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTRENAME"           c_H5E_CANTRENAME           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTOPENOBJ"          c_H5E_CANTOPENOBJ          :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCLOSEOBJ"         c_H5E_CANTCLOSEOBJ         :: HID
foreign import capi "hdf5-hs.h value H5E_COMPLEN"              c_H5E_COMPLEN              :: HID
foreign import capi "hdf5-hs.h value H5E_PATH"                 c_H5E_PATH                 :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCONVERT"          c_H5E_CANTCONVERT          :: HID
foreign import capi "hdf5-hs.h value H5E_BADSIZE"              c_H5E_BADSIZE              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCLIP"             c_H5E_CANTCLIP             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCOUNT"            c_H5E_CANTCOUNT            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTSELECT"           c_H5E_CANTSELECT           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTNEXT"             c_H5E_CANTNEXT             :: HID
foreign import capi "hdf5-hs.h value H5E_BADSELECT"            c_H5E_BADSELECT            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCOMPARE"          c_H5E_CANTCOMPARE          :: HID
foreign import capi "hdf5-hs.h value H5E_INCONSISTENTSTATE"    c_H5E_INCONSISTENTSTATE    :: HID
foreign import capi "hdf5-hs.h value H5E_CANTAPPEND"           c_H5E_CANTAPPEND           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTGET"              c_H5E_CANTGET              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTSET"              c_H5E_CANTSET              :: HID
foreign import capi "hdf5-hs.h value H5E_DUPCLASS"             c_H5E_DUPCLASS             :: HID
foreign import capi "hdf5-hs.h value H5E_SETDISALLOWED"        c_H5E_SETDISALLOWED        :: HID
foreign import capi "hdf5-hs.h value H5E_TRAVERSE"             c_H5E_TRAVERSE             :: HID
foreign import capi "hdf5-hs.h value H5E_NLINKS"               c_H5E_NLINKS               :: HID
foreign import capi "hdf5-hs.h value H5E_NOTREGISTERED"        c_H5E_NOTREGISTERED        :: HID
foreign import capi "hdf5-hs.h value H5E_CANTMOVE"             c_H5E_CANTMOVE             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTSORT"             c_H5E_CANTSORT             :: HID
foreign import capi "hdf5-hs.h value H5E_MPI"                  c_H5E_MPI                  :: HID
foreign import capi "hdf5-hs.h value H5E_MPIERRSTR"            c_H5E_MPIERRSTR            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTRECV"             c_H5E_CANTRECV             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTGATHER"           c_H5E_CANTGATHER           :: HID
foreign import capi "hdf5-hs.h value H5E_NO_INDEPENDENT"       c_H5E_NO_INDEPENDENT       :: HID
foreign import capi "hdf5-hs.h value H5E_CANTRESTORE"          c_H5E_CANTRESTORE          :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCOMPUTE"          c_H5E_CANTCOMPUTE          :: HID
foreign import capi "hdf5-hs.h value H5E_CANTEXTEND"           c_H5E_CANTEXTEND           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTATTACH"           c_H5E_CANTATTACH           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTUPDATE"           c_H5E_CANTUPDATE           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTOPERATE"          c_H5E_CANTOPERATE          :: HID
foreign import capi "hdf5-hs.h value H5E_CANTMERGE"            c_H5E_CANTMERGE            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTREVIVE"           c_H5E_CANTREVIVE           :: HID
foreign import capi "hdf5-hs.h value H5E_CANTSHRINK"           c_H5E_CANTSHRINK           :: HID
foreign import capi "hdf5-hs.h value H5E_NOFILTER"             c_H5E_NOFILTER             :: HID
foreign import capi "hdf5-hs.h value H5E_CALLBACK"             c_H5E_CALLBACK             :: HID
foreign import capi "hdf5-hs.h value H5E_CANAPPLY"             c_H5E_CANAPPLY             :: HID
foreign import capi "hdf5-hs.h value H5E_SETLOCAL"             c_H5E_SETLOCAL             :: HID
foreign import capi "hdf5-hs.h value H5E_NOENCODER"            c_H5E_NOENCODER            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTFILTER"           c_H5E_CANTFILTER           :: HID
foreign import capi "hdf5-hs.h value H5E_SYSERRSTR"            c_H5E_SYSERRSTR            :: HID
foreign import capi "hdf5-hs.h value H5E_OPENERROR"            c_H5E_OPENERROR            :: HID
foreign import capi "hdf5-hs.h value H5E_CANTPUT"              c_H5E_CANTPUT              :: HID
foreign import capi "hdf5-hs.h value H5E_CANTWAIT"             c_H5E_CANTWAIT             :: HID
foreign import capi "hdf5-hs.h value H5E_CANTCANCEL"           c_H5E_CANTCANCEL           :: HID
foreign import capi "hdf5-hs.h value H5E_NONE_MINOR"           c_H5E_NONE_MINOR           :: HID


----------------------------------------------------------------
-- Function pointers
----------------------------------------------------------------

-- | Callback to call in case of error
type H5EAuto = HID -> Ptr () -> IO HErr

-- | Callback for traversing error stack
type H5EWalk = CUInt -> Ptr H5EError -> Ptr () -> IO HErr

foreign import ccall "wrapper"
  makeWalker :: H5EWalk -> IO (FunPtr H5EWalk)

----------------------------------------------------------------
-- Wrapper functions
----------------------------------------------------------------

-- | Closes an error message.
foreign import capi "hdf5-hs.h hs_H5Eclose_msg" h5e_close_msg
  :: HID      -- ^ @err_id@ An error message identifier
  -> HIO HErr -- ^ Returns a non-negative value if successful;
              --   otherwise returns a negative value.

-- | Closes an error stack handle. 
foreign import capi "hdf5-hs.h hs_H5Eclose_stack" h5e_close_stack
  :: HID      -- ^ @stack_id@ Error stack identifier
  -> HIO HErr -- ^ Returns a non-negative value if successful;
              --   otherwise returns a negative value.




-- | Creates a new, empty error stack. Use H5Eclose_stack() to close
--   the error stack identifier returned by this function.
foreign import capi "hdf5-hs.h hs_H5Ecreate_stack" h5e_create_stack
  :: HIO HID -- ^ Returns an error stack identifier if successful;
             --   otherwise returns @H5I_INVALID_HID@.

-- | Returns a copy of the current error stack.
foreign import capi "hdf5-hs.h hs_H5Eget_current_stack" h5e_get_current_stack
  :: HIO HID -- ^ Returns an error stack identifier if successful;
             --   otherwise returns @H5I_INVALID_HID@.

-- | @H5Eget_auto2@ returns the settings for the automatic error stack
--   traversal function, @func@, and its data, @client_data@, that are
--   associated with the error stack specified by @estack_id@.
--
--   Either or both of the func and client_data arguments may be @NULL@,
--   in which case the value is not returned.
--
--   The library initializes its default error stack traversal
--   functions to @H5Eprint1@ and @H5Eprint2@. A call to
--   @H5Eget_auto2@ returns @H5Eprint2@ or the user-defined function
--   passed in through @H5Eset_auto2@.
foreign import ccall "hdf5-hs.h hs_H5Eget_auto2" h5e_get_auto
  :: HID          -- ^ @estack_id@ Error stack identifier
  -> Ptr (FunPtr H5EAuto)
     -- ^ @[out]@ @func@ The function currently set to be called upon
     --   an error condition
  -> Ptr (Ptr ()) -- ^ @client_data@ Data currently set to be passed
                  --   to the error function
  -> HIO HErr
-- NOTE: capi chokes on function pointers

-- | Turns automatic error printing on or off.
--
--   @H5Eset_auto2@ turns on or off automatic printing of errors for
--   the error stack specified with estack_id. An estack_id value of
--   H5E_DEFAULT indicates the current stack.
--
--   When automatic printing is turned on, by the use of a non-null
--   func pointer, any API function which returns an error indication
--   will first call func, passing it client_data as an argument.
--
--   func, a function compliant with the @H5E_auto2_t@ prototype, is
--   defined in the @H5Epublic.h@ source code file as:
--
-- > typedef herr_t (*H5E_auto2_t)(hid_t estack, void *client_data);
--
--   When the library is first initialized, the auto printing function
--   is set to @H5Eprint2()@ (cast appropriately) and client_data is the
--   standard error stream pointer, @stderr@.
--
--   Automatic stack traversal is always in the @H5E_WALK_DOWNWARD@
--   direction.
--
--   Automatic error printing is turned off with a H5Eset_auto2() call
--   with a NULL func pointer.
foreign import capi "hdf5-hs.h hs_H5Eset_auto2" h5e_set_auto
  :: HID            -- ^ @estack_id@ Error stack identifier
  -> FunPtr H5EAuto -- ^ Function to be called upon an error condition
  -> Ptr ()         -- ^ @client_data@ Data passed to the error function
  -> HIO HErr       -- ^ Returns a non-negative value if successful;
                    --   otherwise returns a negative value.



-- | @H5Ewalk2@ walks the error stack specified by err_stack for the
--   current thread and calls the function specified in func for each
--   error along the way.
--
--   If the value of err_stack is @H5E_DEFAULT@, then @H5Ewalk2@ walks
--   the current error stack.
--
--   direction specifies whether the stack is walked from the inside
--   out or the outside in. A value of @H5E_WALK_UPWARD@ means to begin
--   with the most specific error and end at the API; a value of
--   @H5E_WALK_DOWNWARD@ means to start at the API and end at the
--   innermost function where the error was first detected.
--
--   @func@, a function conforming to the @H5E_walk2_t@ prototype,
--   will be called for each error in the error stack. Its arguments
--   will include an index number n (beginning at zero regardless of
--   stack traversal direction), an error stack entry err_desc, and
--   the client_data pointer passed to @H5Eprint@. The @H5E_walk2_t@
--   prototype is as follows:
-- 
-- > typedef herr_t (*H5E_walk2_t)(unsigned n, const H5E_error2_t *err_desc, void *client_data);
foreign import capi "hdf5-hs.h hs_H5Ewalk2" h5e_walk
  :: HID            -- ^ @err_stack@ Error stack identifier 
  -> H5EDirection   -- ^ @direction@ Direction in which the error
                    --   stack is to be walked
  -> FunPtr H5EWalk -- ^ @func@ Function to be called for each error encountered 
  -> Ptr ()         -- ^ @client_data@ Data to be passed to func
  -> HIO HErr       -- ^ Returns a non-negative value if successful;
                    --   otherwise returns a negative value.

-- | @H5Eget_msg@ retrieves the error message including its length and
--   type. The error message is specified by msg_id. The user is
--   responsible for passing in sufficient buffer space for the
--   message. If msg is not NULL and size is greater than zero, the
--   error message of size long is returned. The length of the message
--   is also returned. If NULL is passed in as msg, only the length
--   and type of the message is returned. If the return value is zero,
--   it means there is no message.
foreign import capi "hdf5-hs.h hs_H5Eget_msg" h5e_get_msg
  :: HID         -- ^ @msg_id@ Error message identifier 
  -> Ptr H5EType -- ^ @[out]@ @type@ The type of the error message
  -> Ptr CChar   -- ^ @msg@ Error message buffer
  -> CSize       -- ^ @size@ The length of error message to be returned by this function 
  -> HIO CSize   -- ^ Returns the size of the error message in bytes
                 --   on success; otherwise returns a negative value.

{-
hid_t 	H5Eregister_class (const char *cls_name, const char *lib_name, const char *version)
herr_t 	H5Eunregister_class (hid_t class_id)
hid_t 	H5Ecreate_msg (hid_t cls, H5E_type_t msg_type, const char *msg)
herr_t 	H5Eappend_stack (hid_t dst_stack_id, hid_t src_stack_id, hbool_t close_source_stack)
ssize_t H5Eget_class_name (hid_t class_id, char *name, size_t size)
herr_t 	H5Eset_current_stack (hid_t err_stack_id)
herr_t 	H5Epush2 (hid_t err_stack, const char *file, const char *func, unsigned line, hid_t cls_id, hid_t maj_id, hid_t min_id, const char *msg,...)
herr_t 	H5Epop (hid_t err_stack, size_t count)
herr_t 	H5Eprint2 (hid_t err_stack, FILE *stream)
herr_t 	H5Eclear2 (hid_t err_stack)
herr_t 	H5Eauto_is_v2 (hid_t err_stack, unsigned *is_stack)
ssize_t H5Eget_num (hid_t error_stack_id)
herr_t 	H5Eclear1 (void)
herr_t 	H5Eget_auto1 (H5E_auto1_t *func, void **client_data)
herr_t 	H5Epush1 (const char *file, const char *func, unsigned line, H5E_major_t maj, H5E_minor_t min, const char *str)
herr_t 	H5Eprint1 (FILE *stream)
herr_t 	H5Eset_auto1 (H5E_auto1_t func, void *client_data)
-}
