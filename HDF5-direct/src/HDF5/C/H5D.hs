{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
-- API for datasets
module HDF5.C.H5D
  ( -- * Enums
    H5DLayout(..)
  , pattern H5D_LAYOUT_ERROR
  , pattern H5D_COMPACT
  , pattern H5D_CONTIGUOUS
  , pattern H5D_CHUNKED
  , pattern H5D_VIRTUAL
    -- * Functions
  , h5d_open2
  , h5d_create
  , h5d_close
  , h5d_get_type
  , h5d_get_space
  , h5d_read
  , h5d_write
  ) where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types


-- | Types of dataset layouts
newtype H5DLayout = H5DLayout CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5D_LAYOUT_ERROR" h5d_LAYOUT_ERROR :: H5DLayout
foreign import capi "hdf5.h value H5D_COMPACT"      h5d_COMPACT      :: H5DLayout
foreign import capi "hdf5.h value H5D_CONTIGUOUS"   h5d_CONTIGUOUS   :: H5DLayout
foreign import capi "hdf5.h value H5D_CHUNKED"      h5d_CHUNKED      :: H5DLayout
foreign import capi "hdf5.h value H5D_VIRTUAL"      h5d_VIRTUAL      :: H5DLayout

-- | error
pattern H5D_LAYOUT_ERROR :: H5DLayout
pattern H5D_LAYOUT_ERROR <- ((==h5d_LAYOUT_ERROR) -> True) where H5D_LAYOUT_ERROR = h5d_LAYOUT_ERROR

-- | raw data is small (< 64KB)
pattern H5D_COMPACT :: H5DLayout
pattern H5D_COMPACT <- ((==h5d_COMPACT) -> True) where H5D_COMPACT = h5d_COMPACT

-- | contiguous layout
pattern H5D_CONTIGUOUS :: H5DLayout
pattern H5D_CONTIGUOUS <- ((==h5d_CONTIGUOUS) -> True) where H5D_CONTIGUOUS = h5d_CONTIGUOUS

-- | chunked or tiled layout
pattern H5D_CHUNKED :: H5DLayout
pattern H5D_CHUNKED <- ((==h5d_CHUNKED) -> True) where H5D_CHUNKED = h5d_CHUNKED

-- | actual data is stored in other datasets
pattern H5D_VIRTUAL :: H5DLayout
pattern H5D_VIRTUAL <- ((==h5d_VIRTUAL) -> True) where H5D_VIRTUAL = h5d_VIRTUAL



----------------------------------------------------------------
-- Functions
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
foreign import capi "hdf5-hs.h hs_H5Dopen2" h5d_open2
  :: HID     -- ^ @loc_id@
  -> CString -- ^ Name of dataset to open
  -> HID     -- ^ Dataset access property list identifier
  -> HIO HID

-- | @H5Dcreate2@ creates a new dataset named name at the location
--   specified by loc_id, and associates constant and initial
--   persistent properties with that dataset, including the datatype
--   dtype_id, the dataspace space_id, and other properties as
--   specified by the dataset creation property list dcpl_id and the
--   access property list dapl_id, respectively. Once created, the
--   dataset is opened for access.
--
--   @loc_id@ may specify a file, group, dataset, named datatype, or
--   attribute. If an attribute, dataset, or named datatype is
--   specified then the dataset will be created at the location where
--   the attribute, dataset, or named datatype is attached.
--
---  @name@ may be either an absolute path in the file or a relative
---  path from loc_id naming the dataset.
--
--   If @dtype_id@ is a committed datatype, and if the file location
--   associated with the committed datatype is different from the file
--   location where the dataset will be created, the datatype is
--   copied and converted to a transient type.
--
--   The link creation property list, @lcpl_id@, governs creation of the
--   link(s) by which the new dataset is accessed and the creation of
--   any intermediate groups that may be missing.
--
--   The datatype and dataspace properties and the dataset creation
--   and access property lists are attached to the dataset, so the
--   caller may derive new datatypes, dataspaces, and creation and
--   access properties from the old ones and reuse them in calls to
--   create additional datasets. Once created, the dataset can be read
--   from or written to. Reading data from a dataset that was not
--   previously written, the HDF5 library will return default or
--   user-defined fill values.
foreign import capi "hdf5-hs.h hs_H5Dcreate2" h5d_create
  :: HID     -- ^ @loc_id@ Location identifier. The identifier may be
             --   that of a file, group, dataset, named datatype, or
             --   attribute.
  -> CString -- ^ @name@ Name of the dataset to create
  -> HID     -- ^ @type_id@ Datatype identifier
  -> HID     -- ^ @space_id@ Dataspace identifier
  -> HID     -- ^ @lcpl_id@ Link creation property list identifier
  -> HID     -- ^ @dcpl_id@ Dataset creation property list identifier
  -> HID     -- ^ @dapl_id@ Dataset access property list identifier
  -> HIO HID -- ^ Returns a dataset identifier if successful;
             --   otherwise returns @H5I_INVALID_HID@.


-- | @H5Dclose@ terminates access to a dataset via the identifier
--   @dset_id@ and releases the underlying resources.
--
--   Returns a non-negative value if successful; otherwise returns a
--   negative value.
foreign import capi "hdf5-hs.h hs_H5Dclose" h5d_close
  :: HID    -- ^ Dataset identifier
  -> HIO HErr

-- | @H5Dget_type@ returns an identifier of a copy of the datatype for
--   a dataset.
--
--   If a dataset has a named datatype, then an identifier to the
--   opened datatype is returned. Otherwise, the returned datatype is
--   read-only.
foreign import capi "hdf5-hs.h hs_H5Dget_type" h5d_get_type
  :: HID   -- ^ Dataset identifier
  -> HIO HID


-- | @H5Dget_space@ makes a copy of the dataspace of the dataset
--   specified by @dset_id@. The function returns an identifier for the
--   new copy of the dataspace.
--
--   A dataspace identifier returned from this function should be
--   released with 'h5s_close' when the identifier is no longer needed
--   so that resource leaks will not occur.
--
--   Returns a dataspace identifier if successful; otherwise returns H5I_INVALID_HID
foreign import capi "hdf5-hs.h hs_H5Dget_space" h5d_get_space
  :: HID    -- ^ Dataset identifier
  -> HIO HID

-- | @H5Dread@ reads a dataset, specified by its identifier @dset_id@,
--   from the file into an application memory buffer buf. Data
--   transfer properties are defined by the argument @dxpl_id@. The
--   memory datatype of the (partial) dataset is identified by the
--   identifier @mem_type_id@. The part of the dataset to read is
--   defined by @mem_space_id@ and @file_space_id@.
--
--   @file_space_id@ is used to specify only the selection within the
--   file dataset's dataspace. Any dataspace specified in
--   @file_space_id@ is ignored by the library and the dataset's
--   dataspace is always used. file_space_id can be the constant
--   @H5S_ALL@, which indicates that the entire file dataspace, as
--   defined by the current dimensions of the dataset, is to be
--   selected.
--
--   @mem_space_id@ is used to specify both the memory dataspace and
--   the selection within that dataspace. mem_space_id can be the
--   constant @H5S_ALL@, in which case the file dataspace is used for
--   the memory dataspace and the selection defined with file_space_id
--   is used for the selection within that dataspace.
--
--   The number of elements selected in the memory dataspace must be
--   equal to the number of elements selected in the file dataspace.
foreign import capi "hdf5-hs.h hs_H5Dread" h5d_read
  :: HID  -- ^ @dset_id@ Dataset identifier Identifier of the dataset
          --   to read from
  -> HID  -- ^ @mem_type_id@ Identifier of the memory datatype
  -> HID  -- ^ @mem_space_id@ Identifier of the memory dataspace
  -> HID  -- ^ @file_space_id@ Identifier of the dataset's dataspace in the file
  -> HID  -- ^ @dxpl_id@ Identifier of a transfer property list
  -> Ptr ()
  -> HIO HErr

-- | @H5Dwrite@ writes a (partial) dataset, specified by its
--   identifier @dset_id@, from the application memory buffer buf into
--   the file. Data transfer properties are defined by the argument
--   dxpl_id. The memory datatype of the (partial) dataset is
--   identified by the identifier @mem_type_id@. The part of the dataset
--   to write is defined by mem_space_id and @file_space_id@.
--
--   If mem_type_id is either a fixed-length or variable-length
--   string, it is important to set the string length when defining
--   the datatype. String datatypes are derived from H5T_C_S1 (or
--   H5T_FORTRAN_S1 for Fortran codes), which defaults to 1 character
--   in size. See H5Tset_size() and Creating variable-length string
--   datatypes.
--
--   @file_space_id@ is used to specify only the selection within the
--   file dataset's dataspace. Any dataspace specified in
--   file_space_id is ignored by the library and the dataset's
--   dataspace is always used. file_space_id can be the constant
--   @H5S_ALL@, which indicates that the entire file dataspace, as
--   defined by the current dimensions of the dataset, is to be
--   selected.
--
--   @mem_space_id@ is used to specify both the memory dataspace and
--   the selection within that dataspace. mem_space_id can be the
--   constant @H5S_ALL@, in which case the file dataspace is used for
--   the memory dataspace and the selection defined with file_space_id
--   is used for the selection within that dataspace.
foreign import capi "hdf5-hs.h hs_H5Dwrite" h5d_write
  :: HID   -- ^ @dset_id Identifier of the dataset to read from
  -> HID   -- ^ @mem_type_id Identifier of the memory datatype
  -> HID   -- ^ @mem_space_id Identifier of the memory dataspace
  -> HID   -- ^ @file_space_id@ Identifier of the dataset's dataspace in the file
  -> HID   -- ^ @dxpl_id@ Dataset transfer property list identifier
  -> Ptr x -- ^ @buf@ Buffer with data to be written to the file
  -> HIO HErr

{-
hid_t   H5Dcreate_anon (hid_t loc_id, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t dapl_id)
hid_t   H5Dopen1 (hid_t loc_id, const char *name)
hid_t   H5Dopen2 (hid_t loc_id, const char *name, hid_t dapl_id)
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
herr_t  H5Dread_multi (size_t count, hid_t dset_id[], hid_t mem_type_id[], hid_t mem_space_id[], hid_t file_space_id[], hid_t dxpl_id, void *buf[])
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
