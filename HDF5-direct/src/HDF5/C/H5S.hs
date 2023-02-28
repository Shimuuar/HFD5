{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
-- API for dataspaces
module HDF5.C.H5S
  ( -- * Constants
    h5s_ALL
    -- * Enumerations
    -- ** Dataset class
  , H5SClass(..)
  , pattern H5S_NO_CLASS
  , pattern H5S_SCALAR
  , pattern H5S_SIMPLE
  , pattern H5S_NULL
  , pattern H5S_UNLIMITED
    -- ** Dataset selection operations
  , H5SSelOper(..)
  , pattern H5S_SELECT_NOOP
  , pattern H5S_SELECT_SET
  , pattern H5S_SELECT_OR
  , pattern H5S_SELECT_AND
  , pattern H5S_SELECT_XOR
  , pattern H5S_SELECT_NOTB
  , pattern H5S_SELECT_NOTA
  , pattern H5S_SELECT_APPEND
  , pattern H5S_SELECT_PREPEND
    -- * Functions
  , h5s_close
  , h5s_create
  , h5s_create_simple
  , h5s_is_simple
  , h5s_get_simple_extent_type
  , h5s_get_simple_extent_ndims
  , h5s_get_simple_extent_npoints
  , h5s_get_simple_extent_dims
  , h5s_select_hyperslab
  ) where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types


foreign import capi "hdf5.h value H5S_ALL" h5s_ALL :: HID

----------------------------------------------------------------
-- Enumerations
----------------------------------------------------------------

-- | Types of dataspaces
newtype H5SClass = H5SClass CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5S_NO_CLASS"  h5s_NO_CLASS  :: H5SClass
foreign import capi "hdf5.h value H5S_SCALAR"    h5s_SCALAR    :: H5SClass
foreign import capi "hdf5.h value H5S_SIMPLE"    h5s_SIMPLE    :: H5SClass
foreign import capi "hdf5.h value H5S_NULL"      h5s_NULL      :: H5SClass
foreign import capi "hdf5.h value H5S_UNLIMITED" h5s_UNLIMITED :: HSize

-- | Error
pattern H5S_NO_CLASS :: H5SClass
pattern H5S_NO_CLASS <- ((==h5s_NO_CLASS) -> True) where H5S_NO_CLASS = h5s_NO_CLASS

-- | Singleton (scalar)
pattern H5S_SCALAR :: H5SClass
pattern H5S_SCALAR <- ((==h5s_SCALAR) -> True) where H5S_SCALAR = h5s_SCALAR

-- | Regular grid
pattern H5S_SIMPLE :: H5SClass
pattern H5S_SIMPLE <- ((==h5s_SIMPLE) -> True) where H5S_SIMPLE = h5s_SIMPLE

-- | Empty set
pattern H5S_NULL :: H5SClass
pattern H5S_NULL <- ((==h5s_NULL) -> True) where H5S_NULL = h5s_NULL

-- | Value for 'unlimited' dimensions
pattern H5S_UNLIMITED :: HSize
pattern H5S_UNLIMITED <- ((==h5s_UNLIMITED) -> True) where H5S_UNLIMITED = h5s_UNLIMITED


-- | Different ways of combining selections
newtype H5SSelOper = H5SSelOper CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5S_SELECT_NOOP"    h5s_SELECT_NOOP    :: H5SSelOper
foreign import capi "hdf5.h value H5S_SELECT_SET"     h5s_SELECT_SET     :: H5SSelOper
foreign import capi "hdf5.h value H5S_SELECT_OR"      h5s_SELECT_OR      :: H5SSelOper
foreign import capi "hdf5.h value H5S_SELECT_AND"     h5s_SELECT_AND     :: H5SSelOper
foreign import capi "hdf5.h value H5S_SELECT_XOR"     h5s_SELECT_XOR     :: H5SSelOper
foreign import capi "hdf5.h value H5S_SELECT_NOTB"    h5s_SELECT_NOTB    :: H5SSelOper
foreign import capi "hdf5.h value H5S_SELECT_NOTA"    h5s_SELECT_NOTA    :: H5SSelOper
foreign import capi "hdf5.h value H5S_SELECT_APPEND"  h5s_SELECT_APPEND  :: H5SSelOper
foreign import capi "hdf5.h value H5S_SELECT_PREPEND" h5s_SELECT_PREPEND :: H5SSelOper


-- | Error
pattern H5S_SELECT_NOOP :: H5SSelOper
pattern H5S_SELECT_NOOP <- ((==h5s_SELECT_NOOP) -> True) where H5S_SELECT_NOOP = h5s_SELECT_NOOP

-- | Select @set@ operation
pattern H5S_SELECT_SET :: H5SSelOper
pattern H5S_SELECT_SET <- ((==h5s_SELECT_SET) -> True) where H5S_SELECT_SET = h5s_SELECT_SET

-- | Binary @or@ operation for hyperslabs (add new selection to existing selection)
--
-- > Original region:  AAAAAAAAAA
-- > New region:             BBBBBBBBBB
-- > A or B:           CCCCCCCCCCCCCCCC
pattern H5S_SELECT_OR :: H5SSelOper
pattern H5S_SELECT_OR <- ((==h5s_SELECT_OR) -> True) where H5S_SELECT_OR = h5s_SELECT_OR

-- | Binary @and@ operation for hyperslabs (only leave overlapped
--   regions in selection)
--
-- > Original region:  AAAAAAAAAA
-- > New region:             BBBBBBBBBB
-- > A and B:                CCCC
pattern H5S_SELECT_AND :: H5SSelOper
pattern H5S_SELECT_AND <- ((==h5s_SELECT_AND) -> True) where H5S_SELECT_AND = h5s_SELECT_AND

-- | Binary @xor@ operation for hyperslabs (only leave non-overlapped
--   regions in selection)
--
-- > Original region:  AAAAAAAAAA
-- > New region:             BBBBBBBBBB
-- > A xor B:          CCCCCC    CCCCCC
pattern H5S_SELECT_XOR :: H5SSelOper
pattern H5S_SELECT_XOR <- ((==h5s_SELECT_XOR) -> True) where H5S_SELECT_XOR = h5s_SELECT_XOR

-- | Binary @not@ operation for hyperslabs (only leave non-overlapped
--   regions in original selection)
--
-- > Original region:  AAAAAAAAAA
-- > New region:             BBBBBBBBBB
-- > A not B:          CCCCCC
pattern H5S_SELECT_NOTB :: H5SSelOper
pattern H5S_SELECT_NOTB <- ((==h5s_SELECT_NOTB) -> True) where H5S_SELECT_NOTB = h5s_SELECT_NOTB

-- | Binary @not@ operation for hyperslabs (only leave non-overlapped
--   regions in new selection)
--
-- > Original region:  AAAAAAAAAA
-- > New region:             BBBBBBBBBB
-- > B not A:                    CCCCCC
pattern H5S_SELECT_NOTA :: H5SSelOper
pattern H5S_SELECT_NOTA <- ((==h5s_SELECT_NOTA) -> True) where H5S_SELECT_NOTA = h5s_SELECT_NOTA

-- | Append elements to end of point selection
pattern H5S_SELECT_APPEND :: H5SSelOper
pattern H5S_SELECT_APPEND <- ((==h5s_SELECT_APPEND) -> True) where H5S_SELECT_APPEND = h5s_SELECT_APPEND

-- | Prepend elements to beginning of point selection
pattern H5S_SELECT_PREPEND :: H5SSelOper
pattern H5S_SELECT_PREPEND <- ((==h5s_SELECT_PREPEND) -> True) where H5S_SELECT_PREPEND = h5s_SELECT_PREPEND



----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

-- | @H5Sclose@ releases a dataspace. Further access through the
--   dataspace identifier is illegal. Failure to release a dataspace
--   with this call will result in resource leaks.
--
--   Returns a non-negative value if successful; otherwise returns a
--   negative value.
foreign import capi "hdf5-hs.h hs_H5Sclose" h5s_close
  :: HID     -- ^ Dataspace identifier
  -> HIO HErr

-- | Creates a new dataspace of a specified type.
--
--   A scalar dataspace, H5S_SCALAR, has a single element, though that
--   element may be of a complex datatype, such as a compound or array
--   datatype. By convention, the rank of a scalar dataspace is always
--   0 (zero); think of it geometrically as a single, dimensionless
--   point, though that point can be complex.
--
--   A simple dataspace, H5S_SIMPLE, consists of a regular array of elements.
--
--   A null dataspace, H5S_NULL, has no data elements.
--
--   The dataspace identifier returned by this function can be
--   released with H5Sclose() so that resource leaks will not occur.
foreign import capi "hdf5-hs.h hs_H5Screate" h5s_create
  :: H5SClass -- ^ Type of dataspace to be created
  -> HIO HID

-- | Creates a new simple dataspace and opens it for access.
--
--   @rank@ is the number of dimensions used in the dataspace.
--
--   @dims@ is a one-dimensional array of size rank specifying the size
--   of each dimension of the dataset. maxdims is an array of the same
--   size specifying the upper limit on the size of each dimension.
--
--   Any element of dims can be 0 (zero). Note that no data can be
--   written to a dataset if the size of any dimension of its current
--   dataspace is 0. This is sometimes a useful initial state for a
--   dataset.
--
--   @maxdims@ may be the null pointer, in which case the upper limit
--   is the same as dims. Otherwise, no element of maxdims should be
--   smaller than the corresponding element of dims.
--
--   If an element of maxdims is @H5S_UNLIMITED@, the maximum size of
--   the corresponding dimension is unlimited.
--
--   Any dataset with an unlimited dimension must also be chunked; see
--   @H5Pset_chunk@. Similarly, a dataset must be chunked if dims does
--   not equal maxdims.
--
--   The dataspace identifier returned from this function must be
--   released with @H5Sclose@ or resource leaks will occur.
foreign import capi "hdf5-hs.h hs_H5Screate_simple" h5s_create_simple
  :: CInt      -- ^ @rank@ Number of dimensions of dataspace
  -> Ptr HSize -- ^ @dims@ Array specifying the size of each dimension
  -> Ptr HSize -- ^ @maxdims@ Array specifying the maximum size of each dimension
  -> HIO HID

-- | @H5Sis_simple@ determines whether or not a dataspace is a simple dataspace.
--
--   Returns zero (false), a positive (true) or a negative (failure) value.
foreign import capi "hdf5-hs.h hs_H5Sis_simple" h5s_is_simple
  :: HID
  -> HIO HTri

-- | Returns the number of dimensions in the dataspace if successful;
--   otherwise returns a negative value.
foreign import capi "hdf5-hs.h hs_H5Sget_simple_extent_ndims" h5s_get_simple_extent_ndims
  :: HID    -- ^ Space ID
  -> HIO CInt

-- | @H5Sget_simple_extent_dims@ returns the size and maximum sizes of
--   each dimension of a dataspace space_id through the dims and maxdims
--   parameters.
--
--   Returns the number of dimensions in the dataspace if successful;
--   otherwise returns a negative value.
foreign import capi "hdf5-hs.h hs_H5Sget_simple_extent_dims" h5s_get_simple_extent_dims
  :: HID       -- ^ Space ID
  -> Ptr HSize -- ^ @[out]@ Pointer to array to store the size of each dimension
  -> Ptr HSize -- ^ @[out]@ Pointer to array to store the maximum size of each dimension
  -> HIO CInt

-- | @H5Sget_simple_extent_npoints@ determines the number of elements
--   in a dataspace space_id. For example, a simple 3-dimensional
--   dataspace with dimensions 2, 3, and 4 would have 24 elements.
--
--   Returns the number of elements in the dataspace if successful;
--   otherwise returns a negative value.
foreign import capi "hdf5-hs.h hs_H5Sget_simple_extent_npoints" h5s_get_simple_extent_npoints
  :: HID       -- ^ Dataspace identifier
  -> HIO HSSize

-- | Determines the current class of a dataspace.
foreign import capi "hdf5-hs.h hs_H5Sget_simple_extent_type" h5s_get_simple_extent_type
  :: HID          -- ^ Dataspace identifier
  -> HIO H5SClass -- ^ Returns a dataspace class name if successful; otherwise @H5S_NO_CLASS@ (-1).


-- | @H5Sselect_hyperslab@ selects a hyperslab region to add to the
--   current selected region for the dataspace specified by space_id.
--
--   The start, stride, count, and block arrays must be the same size
--   as the rank of the dataspace. For example, if the dataspace is
--   4-dimensional, each of these parameters must be a 1-dimensional
--   array of size 4.
--
--   The selection operator op determines how the new selection is to
--   be combined with the already existing selection for the
--   dataspace.
--
--   The start array specifies the offset of the starting element of the specified hyperslab.
--
--   The stride array chooses array locations from the dataspace with
--   each value in the stride array determining how many elements to
--   move in each dimension. Setting a value in the stride array to 1
--   moves to each element in that dimension of the dataspace; setting
--   a value of 2 in allocation in the stride array moves to every
--   other element in that dimension of the dataspace. In other words,
--   the stride determines the number of elements to move from the
--   start location in each dimension. Stride values of 0 are not
--   allowed. If the stride parameter is NULL, a contiguous hyperslab
--   is selected (as if each value in the stride array were set to 1).
--
--   The count array determines how many blocks to select from the
--   dataspace, in each dimension.
--
--   The block array determines the size of the element block selected
--   from the dataspace. If the block parameter is set to NULL, the
--   block size defaults to a single element in each dimension (as if
--   each value in the block array were set to 1).
--
--   For example, consider a 2-dimensional dataspace with hyperslab
--   selection settings as follows: the start offset is specified as
--   [1,1], stride is [4,4], count is [3,7], and block is [2,2]. In C,
--   these settings will specify a hyperslab consisting of 21 2x2
--   blocks of array elements starting with location (1,1) with the
--   selected blocks at locations (1,1), (5,1), (9,1), (1,5), (5,5),
--   etc.; in Fortran, they will specify a hyperslab consisting of 21
--   2x2 blocks of array elements starting with location (2,2) with
--   the selected blocks at locations (2,2), (6,2), (10,2), (2,6),
--   (6,6), etc.
--
--   Regions selected with this function call default to C order
--   iteration when I/O is performed.
foreign import capi "hdf5-hs.h hs_H5Sselect_hyperslab" h5s_select_hyperslab
  :: HID        -- ^ @space_id@ Dataspace identifier
  -> H5SSelOper -- ^ @op@ Operation to perform on current selection
  -> Ptr HSize  -- ^ @start@ Offset of start of hyperslab
  -> Ptr HSize  -- ^ @stride@ Hyperslab stride
  -> Ptr HSize  -- ^ @count@ Number of blocks included in hyperslab
  -> Ptr HSize  -- ^ @block@ Size of block in hyperslab
  -> HIO HErr

{-
hid_t        H5Scombine_hyperslab (hid_t space_id, H5S_seloper_t op, const hsize_t start[], const h
hid_t        H5Scombine_select (hid_t space1_id, H5S_seloper_t op, hid_t space2_id)
hid_t        H5Scopy (hid_t space_id)
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

htri_t       H5Sselect_intersect_block (hid_t space_id, const hsize_t *start, const hsize_t *end)
herr_t       H5Sselect_none (hid_t spaceid)
hid_t        H5Sselect_project_intersection (hid_t src_space_id, hid_t dst_space_id, hid_t src_intersect_space_id)
htri_t       H5Sselect_shape_same (hid_t space1_id, hid_t space2_id)
htri_t       H5Sselect_valid (hid_t spaceid)
herr_t       H5Sset_extent_none (hid_t space_id)
herr_t       H5Sset_extent_simple (hid_t space_id, int rank, const hsize_t dims[], const hsize_t max[])
herr_t       H5Sencode1 (hid_t obj_id, void *buf, size_t *nalloc)
-}
