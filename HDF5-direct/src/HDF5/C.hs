{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
module HDF5.C
  ( -- * Wrappers
    HID(..)
  , HTri(..)
  , pattern HTrue
  , pattern HFalse
  , pattern HFail
  , HErr(..)
  , pattern HOK
  , pattern HErrored
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
  , h5d_open2
  , h5d_close
  , h5d_get_type
  , h5t_NATIVE_CHAR
  , h5t_NATIVE_SCHAR
  , h5t_NATIVE_UCHAR
  , h5t_NATIVE_SHORT
  , h5t_NATIVE_USHORT
  , h5t_NATIVE_INT
  , h5t_NATIVE_UINT
  , h5t_NATIVE_LONG
  , h5t_NATIVE_ULONG
  , h5t_NATIVE_LLONG
  , h5t_NATIVE_ULLONG
  , h5t_NATIVE_FLOAT
  , h5t_NATIVE_DOUBLE
  , h5t_NATIVE_LDOUBLE
  , h5t_NATIVE_B8
  , h5t_NATIVE_B16
  , h5t_NATIVE_B32
  , h5t_NATIVE_B64
  , h5t_NATIVE_OPAQUE
  , h5t_NATIVE_HADDR
  , h5t_NATIVE_HSIZE
  , h5t_NATIVE_HSSIZE
  , h5t_NATIVE_HERR
  , h5t_NATIVE_HBOOL
  ) where

import Data.Int
import Foreign.C
import GHC.Generics (Generic)

----------------------------------------------------------------
-- Newtype wrappers
----------------------------------------------------------------

-- | Type of IDs to return to users
newtype HID = HID Int64
  deriving stock (Show,Eq,Ord,Generic)

-- | Three-valued Boolean type. Functions that return htri_t however
--   return zero (false), positive (true), or negative (failure).
newtype HTri = HTri CInt
  deriving stock (Show,Eq,Ord,Generic)

pattern HTrue, HFalse, HFail :: HTri
pattern HFalse = HTri 0
pattern HTrue  <- HTri ((>0) -> True)
pattern HFail  <- HTri ((<0) -> True)
{-# COMPLETE HTrue, HFalse, HFail #-}

-- | Status return values. Failed integer functions in HDF5 result
--   almost always in a negative value (unsigned failing functions
--   sometimes return zero for failure) while successful return is
--   non-negative (often zero). The negative failure value is most
--   commonly -1, but don't bet on it.
newtype HErr = HErr CInt
  deriving stock (Show,Eq,Ord,Generic)

pattern HErrored, HOK :: HErr
pattern HErrored <- HErr ((<0)  -> True)
pattern HOK      <- HErr ((>=0) -> True)
{-# COMPLETE HErrored, HOK #-}


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


----------------------------------------------------------------
-- Working with dataseta
----------------------------------------------------------------

-- | @H5Dopen2@ opens the existing dataset specified by a location
--   identifier and name, loc_id and name, respectively.
--
--   @loc_id@ may specify a file, group, dataset, named datatype, or
--   attribute. If an attribute, dataset, or named datatype is
--   specified then the dataset will be opened at the location where
--   the attribute, dataset, or named datatype is attached.
--
--   Returns a dataset identifier if successful; otherwise returns
--   @H5I_INVALID_HID@.
foreign import capi "hdf5.h H5Dopen2" h5d_open2
  :: HID     -- ^ @loc_id@
  -> CString -- ^ Name of dataset to open
  -> HID     -- ^ Dataset access property list identifier
  -> IO HID


-- | @H5Dclose@ terminates access to a dataset via the identifier
--   @dset_id@ and releases the underlying resources.
--
--   Returns a non-negative value if successful; otherwise returns a
--   negative value.
foreign import capi "hdf5.h H5Dclose" h5d_close
  :: HID    -- ^ Dataset identifier
  -> IO HErr

-- | @H5Dget_type@ returns an identifier of a copy of the datatype for
--   a dataset.
--
--   If a dataset has a named datatype, then an identifier to the
--   opened datatype is returned. Otherwise, the returned datatype is
--   read-only.
foreign import capi "hdf5.h H5Dget_type" h5d_get_type
  :: HID   -- ^ Dataset identifier
  -> IO HID

{-
hid_t   H5Dcreate2 (hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id)
hid_t   H5Dcreate_anon (hid_t loc_id, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t dapl_id)
hid_t   H5Dopen1 (hid_t loc_id, const char *name)
hid_t   H5Dopen2 (hid_t loc_id, const char *name, hid_t dapl_id)
hid_t   H5Dget_space (hid_t dset_id)
herr_t  H5Dget_space_status (hid_t dset_id, H5D_space_status_t *allocation)
hid_t   H5Dget_create_plist (hid_t dset_id)
hid_t   H5Dget_access_plist (hid_t dset_id)
hsize_t H5Dget_storage_size (hid_t dset_id)
herr_t  H5Dget_chunk_storage_size (hid_t dset_id, const hsize_t *offset, hsize_t *chunk_bytes)
herr_t  H5Dget_num_chunks (hid_t dset_id, hid_t fspace_id, hsize_t *nchunks)
herr_t  H5Dget_chunk_info_by_coord (hid_t dset_id, const hsize_t *offset, unsigned *filter_mask, haddr_t *addr, hsize_t *size)
herr_t  H5Dchunk_iter (hid_t dset_id, hid_t dxpl_id, H5D_chunk_iter_op_t cb, void *op_data)
herr_t  H5Dget_chunk_info (hid_t dset_id, hid_t fspace_id, hsize_t chk_idx, hsize_t *offset, unsigned *filter_mask, haddr_t *addr, hsize_t *size)
haddr_t H5Dget_offset (hid_t dset_id)
herr_t  H5Dread (hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, void *buf)
herr_t  H5Dread_multi (size_t count, hid_t dset_id[], hid_t mem_type_id[], hid_t mem_space_id[], hid_t file_space_id[], hid_t dxpl_id, void *buf[])
herr_t  H5Dwrite (hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t dxpl_id, const void *buf)
herr_t  H5Dwrite_multi (size_t count, hid_t dset_id[], hid_t mem_type_id[], hid_t mem_space_id[], hid_t file_space_id[], hid_t dxpl_id, const void *buf[])
herr_t  H5Dwrite_chunk (hid_t dset_id, hid_t dxpl_id, uint32_t filters, const hsize_t *offset, size_t data_size, const void *buf)
herr_t  H5Dread_chunk (hid_t dset_id, hid_t dxpl_id, const hsize_t *offset, uint32_t *filters, void *buf)
herr_t  H5Diterate (void *buf, hid_t type_id, hid_t space_id, H5D_operator_t op, void *operator_data)
herr_t  H5Dvlen_get_buf_size (hid_t dset_id, hid_t type_id, hid_t space_id, hsize_t *size)
herr_t  H5Dfill (const void *fill, hid_t fill_type_id, void *buf, hid_t buf_type_id, hid_t space_id)
herr_t  H5Dset_extent (hid_t dset_id, const hsize_t size[])
herr_t  H5Dflush (hid_t dset_id)
herr_t  H5Drefresh (hid_t dset_id)
herr_t  H5Dscatter (H5D_scatter_func_t op, void *op_data, hid_t type_id, hid_t dst_space_id, void *dst_buf)
herr_t  H5Dgather (hid_t src_space_id, const void *src_buf, hid_t type_id, size_t dst_buf_size, void *dst_buf, H5D_gather_func_t op, void *op_data)
herr_t  H5Dextend (hid_t dset_id, const hsize_t size[])
herr_t  H5Dvlen_reclaim (hid_t type_id, hid_t space_id, hid_t dxpl_id, void *buf)
-}


----------------------------------------------------------------
-- Types API
----------------------------------------------------------------

-- | C-style char
foreign import capi "hdf5.h value H5T_NATIVE_CHAR" h5t_NATIVE_CHAR :: HID

-- | C-style signed char
foreign import capi "hdf5.h value H5T_NATIVE_SCHAR" h5t_NATIVE_SCHAR :: HID

-- | C-style unsigned signed char
foreign import capi "hdf5.h value H5T_NATIVE_UCHAR" h5t_NATIVE_UCHAR :: HID

-- | C-style short
foreign import capi "hdf5.h value H5T_NATIVE_SHORT" h5t_NATIVE_SHORT :: HID

-- | C-style unsigned short
foreign import capi "hdf5.h value H5T_NATIVE_USHORT" h5t_NATIVE_USHORT :: HID

-- | C-style int
foreign import capi "hdf5.h value H5T_NATIVE_INT" h5t_NATIVE_INT :: HID

-- | C-style unsigned int
foreign import capi "hdf5.h value H5T_NATIVE_UINT" h5t_NATIVE_UINT :: HID

-- | C-style long
foreign import capi "hdf5.h value H5T_NATIVE_LONG" h5t_NATIVE_LONG :: HID

-- | C-style unsigned long
foreign import capi "hdf5.h value H5T_NATIVE_ULONG" h5t_NATIVE_ULONG :: HID

-- | C-style long long
foreign import capi "hdf5.h value H5T_NATIVE_LLONG" h5t_NATIVE_LLONG :: HID

-- | C-style unsigned long long
foreign import capi "hdf5.h value H5T_NATIVE_ULLONG" h5t_NATIVE_ULLONG :: HID

-- | C-style float
foreign import capi "hdf5.h value H5T_NATIVE_FLOAT" h5t_NATIVE_FLOAT :: HID

-- | C-style double
foreign import capi "hdf5.h value H5T_NATIVE_DOUBLE" h5t_NATIVE_DOUBLE :: HID

-- | C-style long double
foreign import capi "hdf5.h value H5T_NATIVE_LDOUBLE" h5t_NATIVE_LDOUBLE :: HID

-- | 8-bit bitfield based on native types
foreign import capi "hdf5.h value H5T_NATIVE_B8" h5t_NATIVE_B8 :: HID

-- | 16-bit bitfield based on native types
foreign import capi "hdf5.h value H5T_NATIVE_B16" h5t_NATIVE_B16 :: HID

-- | 32-bit bitfield based on native types
foreign import capi "hdf5.h value H5T_NATIVE_B32" h5t_NATIVE_B32 :: HID

-- | 64-bit bitfield based on native types
foreign import capi "hdf5.h value H5T_NATIVE_B64" h5t_NATIVE_B64 :: HID

-- | opaque unit based on native types
foreign import capi "hdf5.h value H5T_NATIVE_OPAQUE" h5t_NATIVE_OPAQUE :: HID

-- | address type based on native types
foreign import capi "hdf5.h value H5T_NATIVE_HADDR" h5t_NATIVE_HADDR :: HID

-- | size type based on native types
foreign import capi "hdf5.h value H5T_NATIVE_HSIZE" h5t_NATIVE_HSIZE :: HID

-- | signed size type based on native types
foreign import capi "hdf5.h value H5T_NATIVE_HSSIZE" h5t_NATIVE_HSSIZE :: HID

-- | error code type based on native types
foreign import capi "hdf5.h value H5T_NATIVE_HERR" h5t_NATIVE_HERR :: HID

-- | Boolean type based on native types
foreign import capi "hdf5.h value H5T_NATIVE_HBOOL" h5t_NATIVE_HBOOL :: HID

