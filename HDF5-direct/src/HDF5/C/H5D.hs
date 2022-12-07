{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- API for datasets
module HDF5.C.H5D
  ( -- * Functions
    h5d_open2
  , h5d_close
  , h5d_get_type
  , h5d_get_space
  ) where

import Foreign.C
import HDF5.C.Types


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


-- | @H5Dget_space@ makes a copy of the dataspace of the dataset
--   specified by @dset_id@. The function returns an identifier for the
--   new copy of the dataspace.
--
--   A dataspace identifier returned from this function should be 
--   released with 'h5s_close' when the identifier is no longer needed
--   so that resource leaks will not occur.
--
--   Returns a dataspace identifier if successful; otherwise returns H5I_INVALID_HID
foreign import capi "hdf5.h H5Dget_space" h5d_get_space
  :: HID    -- ^ Dataset identifier
  -> IO HID

{-
hid_t   H5Dcreate2 (hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id)
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
