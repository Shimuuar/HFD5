{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- API for dataspaces
module HDF5.C.H5S
  ( -- * Functions
    h5s_close
  , h5s_is_simple
  , h5s_get_simple_extent_ndims
  , h5s_get_simple_extent_npoints
  , h5s_get_simple_extent_dims
  ) where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types


----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

-- | @H5Sclose@ releases a dataspace. Further access through the
--   dataspace identifier is illegal. Failure to release a dataspace
--   with this call will result in resource leaks.
--
--   Returns a non-negative value if successful; otherwise returns a
--   negative value.
foreign import capi "hdf5.h H5Sclose" h5s_close
  :: HID     -- ^ Dataspace identifier
  -> IO HErr

-- | @H5Sis_simple@ determines whether or not a dataspace is a simple dataspace.
--
--   Returns zero (false), a positive (true) or a negative (failure) value.
foreign import capi "hdf5.h H5Sis_simple" h5s_is_simple
  :: HID
  -> IO HTri

-- | Returns the number of dimensions in the dataspace if successful;
--   otherwise returns a negative value.
foreign import capi "hdf5.h H5Sget_simple_extent_ndims" h5s_get_simple_extent_ndims
  :: HID    -- ^ Space ID 
  -> IO CInt

-- | @H5Sget_simple_extent_dims@ returns the size and maximum sizes of
--   each dimension of a dataspace space_id through the dims and maxdims
--   parameters.
--
--   Returns the number of dimensions in the dataspace if successful;
--   otherwise returns a negative value.
foreign import capi "hdf5.h H5Sget_simple_extent_dims" h5s_get_simple_extent_dims
  :: HID       -- ^ Space ID
  -> Ptr HSize -- ^ @[out]@ Pointer to array to store the size of each dimension
  -> Ptr HSize -- ^ @[out]@ Pointer to array to store the maximum size of each dimension
  -> IO CInt

-- | @H5Sget_simple_extent_npoints@ determines the number of elements
--   in a dataspace space_id. For example, a simple 3-dimensional
--   dataspace with dimensions 2, 3, and 4 would have 24 elements.
--
--   Returns the number of elements in the dataspace if successful;
--   otherwise returns a negative value.
foreign import capi "hdf5.h H5Sget_simple_extent_npoints" h5s_get_simple_extent_npoints
  :: HID       -- ^ Space ID
  -> IO HSSize


{-
hid_t        H5Scombine_hyperslab (hid_t space_id, H5S_seloper_t op, const hsize_t start[], const h
hid_t        H5Scombine_select (hid_t space1_id, H5S_seloper_t op, hid_t space2_id)
hid_t        H5Scopy (hid_t space_id)
hid_t        H5Screate (H5S_class_t type)
hid_t        H5Screate_simple (int rank, const hsize_t dims[], const hsize_t maxdims[])
hid_t        H5Sdecode (const void *buf)
herr_t       H5Sencode2 (hid_t obj_id, void *buf, size_t *nalloc, hid_t fapl)
herr_t       H5Sextent_copy (hid_t dst_id, hid_t src_id)
htri_t       H5Sextent_equal (hid_t space1_id, hid_t space2_id)
htri_t       H5Sget_regular_hyperslab (hid_t spaceid, hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[])
herr_t       H5Sget_select_bounds (hid_t spaceid, hsize_t start[], hsize_t end[])
hssize_t     H5Sget_select_elem_npoints (hid_t spaceid)
herr_t       H5Sget_select_elem_pointlist (hid_t spaceid, hsize_t startpoint, hsize_t numpoints, hsize_t buf[])
herr_t       H5Sget_select_hyper_blocklist (hid_t spaceid, hsize_t startblock, hsize_t numblocks, hsize_t buf[])
hssize_t     H5Sget_select_hyper_nblocks (hid_t spaceid)
hssize_t     H5Sget_select_npoints (hid_t spaceid)
H5S_sel_type H5Sget_select_type (hid_t spaceid)
H5S_class_t  H5Sget_simple_extent_type (hid_t space_id)
htri_t       H5Sis_regular_hyperslab (hid_t spaceid)
herr_t       H5Smodify_select (hid_t space1_id, H5S_seloper_t op, hid_t space2_id)
herr_t       H5Soffset_simple (hid_t space_id, const hssize_t *offset)
herr_t       H5Ssel_iter_close (hid_t sel_iter_id)
hid_t        H5Ssel_iter_create (hid_t spaceid, size_t elmt_size, unsigned flags)
herr_t       H5Ssel_iter_get_seq_list (hid_t sel_iter_id, size_t maxseq, size_t maxbytes, size_t *nseq, size_t *nbytes, hsize_t *off, size_t *len)
herr_t       H5Ssel_iter_reset (hid_t sel_iter_id, hid_t space_id)
herr_t       H5Sselect_adjust (hid_t spaceid, const hssize_t *offset)
herr_t       H5Sselect_all (hid_t spaceid)
herr_t       H5Sselect_copy (hid_t dst_id, hid_t src_id)
herr_t       H5Sselect_elements (hid_t space_id, H5S_seloper_t op, size_t num_elem, const hsize_t *coord)
herr_t       H5Sselect_hyperslab (hid_t space_id, H5S_seloper_t op, const hsize_t start[], const hsize_t stride[], const hsize_t count[], const hsize_t block[])
htri_t       H5Sselect_intersect_block (hid_t space_id, const hsize_t *start, const hsize_t *end)
herr_t       H5Sselect_none (hid_t spaceid)
hid_t        H5Sselect_project_intersection (hid_t src_space_id, hid_t dst_space_id, hid_t src_intersect_space_id)
htri_t       H5Sselect_shape_same (hid_t space1_id, hid_t space2_id)
htri_t       H5Sselect_valid (hid_t spaceid)
herr_t       H5Sset_extent_none (hid_t space_id)
herr_t       H5Sset_extent_simple (hid_t space_id, int rank, const hsize_t dims[], const hsize_t max[])
herr_t       H5Sencode1 (hid_t obj_id, void *buf, size_t *nalloc)
-}
