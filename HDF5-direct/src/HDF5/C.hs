{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
module HDF5.C
  ( -- * Data types
    module HDF5.C.Types
    -- * Properties API
    -- ** Constants
  , h5p_DEFAULT
  , h5i_INVALID_HID
    -- * File API
    -- ** Constants
  , h5f_ACC_TRUNC
  , h5f_ACC_EXCL
  , h5f_ACC_RDONLY
  , h5f_ACC_RDWR
  , h5f_ACC_SWMR_WRITE
  , h5f_ACC_SWMR_READ
    -- ** Functions
  , h5f_is_accessible
  , h5f_create
  , h5f_open
  , h5f_reopen
  , h5f_close
  , h5f_delete
    -- * Dataset API
  , module HDF5.C.D
    -- * Datatypes API
  , module HDF5.C.T
    -- * Dataspace API
  , module HDF5.C.S
  ) where

import Foreign.C

import HDF5.C.Types
import HDF5.C.T
import HDF5.C.D
import HDF5.C.S


----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

foreign import capi "hdf5.h value H5P_DEFAULT"        h5p_DEFAULT     :: HID
foreign import capi "hdf5.h value H5I_INVALID_HID"    h5i_INVALID_HID :: HID

foreign import capi "hdf5.h value H5F_ACC_TRUNC"      h5f_ACC_TRUNC      :: CUInt
foreign import capi "hdf5.h value H5F_ACC_EXCL"       h5f_ACC_EXCL       :: CUInt
foreign import capi "hdf5.h value H5F_ACC_RDONLY"     h5f_ACC_RDONLY     :: CUInt
foreign import capi "hdf5.h value H5F_ACC_RDWR"       h5f_ACC_RDWR       :: CUInt
foreign import capi "hdf5.h value H5F_ACC_SWMR_WRITE" h5f_ACC_SWMR_WRITE :: CUInt
foreign import capi "hdf5.h value H5F_ACC_SWMR_READ"  h5f_ACC_SWMR_READ  :: CUInt


----------------------------------------------------------------
-- Functions for working with files
----------------------------------------------------------------

-- | Checks if a file can be opened with a given file access property
--   list. Returns zero (false), a positive (true) or a negative
--   (failure) value.
foreign import capi "hdf5.h H5Fis_accessible" h5f_is_accessible
  :: CString -- ^ Name of a file
  -> HID     -- ^ File access property list identifier
  -> IO HTri

-- | @H5Fcreate@ is the primary function for creating HDF5 files; it
--   creates a new HDF5 file with the specified name and property lists.
foreign import capi "hdf5.h H5Fcreate" h5f_create
  :: CString
  -- ^ Name of file to create
  -> CUInt
  -- ^ File access flags. Allowable values are:
  --
  --   @H5F_ACC_TRUNC@: Truncate file, if it already exists, erasing
  --                    all data previously stored in the file
  --   @H5F_ACC_EXCL@:  Fail if file already exists
  -> HID
  -- ^ @fcpl_id@ File creation property list identifier
  -> HID
  -- ^ @fapl_id@ File access property list identifier
  -> IO HID

-- | @H5Fopen@ is the primary function for accessing existing HDF5
--   files. This function opens the named file in the specified access
--   mode and with the specified access property list.
--
--   Note that @H5Fopen@ does not create a file if it does not already
--   exist; see 'h5f_create'.
--
--   Returns a file identifier if successful; otherwise returns
--   'h5i_INVALID_HID'.
foreign import capi "hdf5.h H5Fopen" h5f_open
  :: CString
  -- ^ Name of the file to be opened
  -> CUInt
  -- ^ File access flags. Allowable values are:
  --
  --   @H5F_ACC_RDWR@: Allows read and write access to file
  --
  --   @H5F_ACC_RDONLY@: Allows read-only access to file
  --
  --   @H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE@: Indicates that the file
  --      is open for writing in a single-writer\/multi-writer (SWMR)
  --      scenario.
  --
  --   @H5F_ACC_RDONLY | H5F_ACC_SWMR_READ@: Indicates that the file
  --   is open for reading in a single-writer/multi-reader (SWMR)
  --   scenario.
  -> HID
  -- ^ File access property list identifier
  -> IO HID

-- | returns a new file identifier for an already-open HDF5 file, as
--   specified by file_id. Both identifiers share caches and other
--   information. The only difference between the identifiers is that
--   the new identifier is not mounted anywhere and no files are
--   mounted on it.
foreign import capi "hdf5.h H5Freopen" h5f_reopen
  :: HID    -- ^ Identifier of a file for which an additional identifier is required
  -> IO HID


-- | Terminates access to an HDF5 file (specified by file_id) by
--   flushing all data to storage.
--
--   If this is the last file identifier open for the file and no
--   other access identifier is open (e.g., a dataset identifier,
--   group identifier, or shared datatype identifier), the file will
--   be fully closed and access will end.
foreign import capi "hdf5.h H5Fclose" h5f_close
  :: HID -- | File identifier
  -> IO HErr

-- | Deletes an HDF5 file filename with a file access property list
--   @fapl_id@. The @fapl_id@ should be configured with the same VOL
--   connector or VFD that was used to open the file.
--
--   This API was introduced for use with the Virtual Object Layer
--   (VOL). With the VOL, HDF5 "files" can map to arbitrary storage
--   schemes such as object stores and relational database tables. The
--   data created by these implementations may be inconvenient for a
--   user to remove without a detailed knowledge of the storage
--   scheme. H5Fdelete() gives VOL connector authors the ability to
--   add connector-specific delete code to their connectors so that
--   users can remove these "files" without detailed knowledge of the
--   storage scheme.
--
--   For a VOL connector, H5Fdelete() deletes the file in a way that
--   makes sense for the specified VOL connector.
--
--   For the native HDF5 connector, HDF5 files will be deleted via the
--   VFDs, each of which will have to be modified to delete the files
--   it creates.
--
--   For all implementations, H5Fdelete() will first check if the file
--   is an HDF5 file via H5Fis_accessible(). This is done to ensure
--   that H5Fdelete() cannot be used as an arbitrary file deletion
--   call.
foreign import capi "hdf5.h H5Fdelete" h5f_delete
  :: CString -- ^ Name of file to delete
  -> HID     -- ^ @fapl_id@ File access property list identifier
  -> IO HErr

{-
herr_t   H5Fflush (hid_t object_id, H5F_scope_t scope)
hid_t    H5Fget_create_plist (hid_t file_id)
hid_t    H5Fget_access_plist (hid_t file_id)
herr_t   H5Fget_intent (hid_t file_id, unsigned *intent)
herr_t   H5Fget_fileno (hid_t file_id, unsigned long *fileno)
ssize_t  H5Fget_obj_count (hid_t file_id, unsigned types)
ssize_t  H5Fget_obj_ids (hid_t file_id, unsigned types, size_t max_objs, hid_t *obj_id_list)
herr_t   H5Fget_vfd_handle (hid_t file_id, hid_t fapl, void **file_handle)
herr_t   H5Fmount (hid_t loc, const char *name, hid_t child, hid_t plist)
herr_t   H5Funmount (hid_t loc, const char *name)
hssize_t H5Fget_freespace (hid_t file_id)
herr_t   H5Fget_filesize (hid_t file_id, hsize_t *size)
herr_t   H5Fget_eoa (hid_t file_id, haddr_t *eoa)
herr_t   H5Fincrement_filesize (hid_t file_id, hsize_t increment)
ssize_t  H5Fget_file_image (hid_t file_id, void *buf_ptr, size_t buf_len)
ssize_t  H5Fget_name (hid_t obj_id, char *name, size_t size)
herr_t   H5Fget_info2 (hid_t obj_id, H5F_info2_t *file_info)
ssize_t  H5Fget_free_sections (hid_t file_id, H5F_mem_t type, size_t nsects, H5F_sect_info_t *sect_info)
herr_t   H5Fclear_elink_file_cache (hid_t file_id)
herr_t   H5Fset_libver_bounds (hid_t file_id, H5F_libver_t low, H5F_libver_t high)
herr_t   H5Freset_page_buffering_stats (hid_t file_id)
herr_t   H5Fget_page_buffering_stats (hid_t file_id, unsigned accesses[2], unsigned hits[2], unsigned misses[2], unsigned evictions[2], unsigned bypasses[2])
herr_t   H5Fget_dset_no_attrs_hint (hid_t file_id, hbool_t *minimize)
herr_t   H5Fset_dset_no_attrs_hint (hid_t file_id, hbool_t minimize)
herr_t   H5Fget_info1 (hid_t obj_id, H5F_info1_t *file_info)
herr_t   H5Fset_latest_format (hid_t file_id, hbool_t latest_format)
htri_t   H5Fis_hdf5 (const char *file_name)
-}
