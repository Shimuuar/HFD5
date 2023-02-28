{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
-- API for property lists
module HDF5.C.H5P
  ( -- * Constants
    pattern H5P_DEFAULT
    -- ** Property list classes
  , pattern H5P_ATTRIBUTE_CREATE
  , pattern H5P_DATASET_ACCESS
  , pattern H5P_DATASET_CREATE
  , pattern H5P_DATASET_XFER
  , pattern H5P_DATATYPE_ACCESS
  , pattern H5P_DATATYPE_CREATE
  , pattern H5P_FILE_ACCESS
  , pattern H5P_FILE_CREATE
  , pattern H5P_FILE_MOUNT
  , pattern H5P_GROUP_ACCESS
  , pattern H5P_GROUP_CREATE
  , pattern H5P_LINK_ACCESS
  , pattern H5P_LINK_CREATE
  , pattern H5P_OBJECT_COPY
  , pattern H5P_OBJECT_CREATE
  , pattern H5P_STRING_CREATE
  , pattern H5P_VOL_INITIALIZE
    -- * Function
  , h5p_close
  , h5p_create
  , h5p_set_layout
  , h5p_set_chunk
  , h5p_set_chunk_opts
  , h5p_set_deflate
  ) where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types
import HDF5.C.H5D (H5DLayout(..))

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

foreign import capi "hdf5.h value H5P_DEFAULT" h5p_DEFAULT :: HID

-- | Class name @attribute create@: Properties for attribute creation
pattern H5P_DEFAULT :: HID
pattern H5P_DEFAULT <- ((==h5p_DEFAULT) -> True) where H5P_DEFAULT = h5p_DEFAULT


foreign import capi "hdf5.h value H5P_ATTRIBUTE_CREATE" h5p_ATTRIBUTE_CREATE :: HID
foreign import capi "hdf5.h value H5P_DATASET_ACCESS"   h5p_DATASET_ACCESS   :: HID
foreign import capi "hdf5.h value H5P_DATASET_CREATE"   h5p_DATASET_CREATE   :: HID
foreign import capi "hdf5.h value H5P_DATASET_XFER"     h5p_DATASET_XFER     :: HID
foreign import capi "hdf5.h value H5P_DATATYPE_ACCESS"  h5p_DATATYPE_ACCESS  :: HID
foreign import capi "hdf5.h value H5P_DATATYPE_CREATE"  h5p_DATATYPE_CREATE  :: HID
foreign import capi "hdf5.h value H5P_FILE_ACCESS"      h5p_FILE_ACCESS      :: HID
foreign import capi "hdf5.h value H5P_FILE_CREATE"      h5p_FILE_CREATE      :: HID
foreign import capi "hdf5.h value H5P_FILE_MOUNT"       h5p_FILE_MOUNT       :: HID
foreign import capi "hdf5.h value H5P_GROUP_ACCESS"     h5p_GROUP_ACCESS     :: HID
foreign import capi "hdf5.h value H5P_GROUP_CREATE"     h5p_GROUP_CREATE     :: HID
foreign import capi "hdf5.h value H5P_LINK_ACCESS"      h5p_LINK_ACCESS      :: HID
foreign import capi "hdf5.h value H5P_LINK_CREATE"      h5p_LINK_CREATE      :: HID
foreign import capi "hdf5.h value H5P_OBJECT_COPY"      h5p_OBJECT_COPY      :: HID
foreign import capi "hdf5.h value H5P_OBJECT_CREATE"    h5p_OBJECT_CREATE    :: HID
foreign import capi "hdf5.h value H5P_STRING_CREATE"    h5p_STRING_CREATE    :: HID
foreign import capi "hdf5.h value H5P_VOL_INITIALIZE"   h5p_VOL_INITIALIZE   :: HID

-- | Class name @attribute create@: Properties for attribute creation
pattern H5P_ATTRIBUTE_CREATE :: HID
pattern H5P_ATTRIBUTE_CREATE <- ((==h5p_ATTRIBUTE_CREATE) -> True) where H5P_ATTRIBUTE_CREATE = h5p_ATTRIBUTE_CREATE

-- | Class name @dataset access@: Properties for dataset access
pattern H5P_DATASET_ACCESS :: HID
pattern H5P_DATASET_ACCESS <- ((==h5p_DATASET_ACCESS) -> True) where H5P_DATASET_ACCESS = h5p_DATASET_ACCESS

-- | Class name @dataset create@: Properties for dataset creation
pattern H5P_DATASET_CREATE :: HID
pattern H5P_DATASET_CREATE <- ((==h5p_DATASET_CREATE) -> True) where H5P_DATASET_CREATE = h5p_DATASET_CREATE

-- | Class name @data transfer@: Properties for raw data transfer
pattern H5P_DATASET_XFER :: HID
pattern H5P_DATASET_XFER <- ((==h5p_DATASET_XFER) -> True) where H5P_DATASET_XFER = h5p_DATASET_XFER

-- | Class name @datatype access@: Properties for datatype access
pattern H5P_DATATYPE_ACCESS :: HID
pattern H5P_DATATYPE_ACCESS <- ((==h5p_DATATYPE_ACCESS) -> True) where H5P_DATATYPE_ACCESS = h5p_DATATYPE_ACCESS

-- | Class name @datatype create@: Properties for datatype creation
pattern H5P_DATATYPE_CREATE :: HID
pattern H5P_DATATYPE_CREATE <- ((==h5p_DATATYPE_CREATE) -> True) where H5P_DATATYPE_CREATE = h5p_DATATYPE_CREATE

-- | Class name @file access@: Properties for file access
pattern H5P_FILE_ACCESS :: HID
pattern H5P_FILE_ACCESS <- ((==h5p_FILE_ACCESS) -> True) where H5P_FILE_ACCESS = h5p_FILE_ACCESS

-- | Class name @file create@: Properties for file creation
pattern H5P_FILE_CREATE :: HID
pattern H5P_FILE_CREATE <- ((==h5p_FILE_CREATE) -> True) where H5P_FILE_CREATE = h5p_FILE_CREATE

-- | Class name @file mount@: Properties for file mounting
pattern H5P_FILE_MOUNT :: HID
pattern H5P_FILE_MOUNT <- ((==h5p_FILE_MOUNT) -> True) where H5P_FILE_MOUNT = h5p_FILE_MOUNT

-- | Class name @group access@: Properties for group access
pattern H5P_GROUP_ACCESS :: HID
pattern H5P_GROUP_ACCESS <- ((==h5p_GROUP_ACCESS) -> True) where H5P_GROUP_ACCESS = h5p_GROUP_ACCESS

-- | Class name @group create@: Properties for group creation
pattern H5P_GROUP_CREATE :: HID
pattern H5P_GROUP_CREATE <- ((==h5p_GROUP_CREATE) -> True) where H5P_GROUP_CREATE = h5p_GROUP_CREATE

-- | Class name @link access@: Properties governing link traversal when accessing objects
pattern H5P_LINK_ACCESS :: HID
pattern H5P_LINK_ACCESS <- ((==h5p_LINK_ACCESS) -> True) where H5P_LINK_ACCESS = h5p_LINK_ACCESS

-- | Class name @link create@: Properties governing link creation
pattern H5P_LINK_CREATE :: HID
pattern H5P_LINK_CREATE <- ((==h5p_LINK_CREATE) -> True) where H5P_LINK_CREATE = h5p_LINK_CREATE

-- | Class name @object copy@: Properties governing the object copying process
pattern H5P_OBJECT_COPY :: HID
pattern H5P_OBJECT_COPY <- ((==h5p_OBJECT_COPY) -> True) where H5P_OBJECT_COPY = h5p_OBJECT_COPY

-- | Class name @object create@: Properties for object creation
pattern H5P_OBJECT_CREATE :: HID
pattern H5P_OBJECT_CREATE <- ((==h5p_OBJECT_CREATE) -> True) where H5P_OBJECT_CREATE = h5p_OBJECT_CREATE

-- | Class name @string create@: Properties for character encoding when encoding strings or object names
pattern H5P_STRING_CREATE :: HID
pattern H5P_STRING_CREATE <- ((==h5p_STRING_CREATE) -> True) where H5P_STRING_CREATE = h5p_STRING_CREATE

-- | Class name @vol initialize@: Properties for VOL initialization
pattern H5P_VOL_INITIALIZE :: HID
pattern H5P_VOL_INITIALIZE <- ((==h5p_VOL_INITIALIZE) -> True) where H5P_VOL_INITIALIZE = h5p_VOL_INITIALIZE


----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

-- | @H5Pclose@ terminates access to a property list. All property
--   lists should be closed when the application is finished accessing
--   them. This frees resources used by the property list.
foreign import capi "hdf5-hs.h hs_H5Pclose" h5p_close
  :: HID
  -> HIO HErr


-- | @H5Pcreate@ creates a new property list as an instance of some
--   property list class. The new property list is initialized with
--   default values for the specified class.
foreign import capi "hdf5-hs.h hs_H5Pcreate" h5p_create
  :: HID     -- ^ @cls_id@ Property list class identifier
  -> HIO HID -- ^ Returns a property list identifier if successful;
             --   otherwise returns @H5I_INVALID_HID@.


-- | @H5Pset_layout@ sets the type of storage used to store the raw
--   data for a dataset. This function is only valid for dataset
--   creation property lists.
--
--   Valid values for layout are:
--
--   * @H5D_COMPACT@: Store raw data in the dataset object header in
--     file. This should only be used for datasets with small amounts
--     of raw data. The raw data size limit is 64K (65520
--     bytes). Attempting to create a dataset with raw data larger
--     than this limit will cause the @H5Dcreate@ call to fail.
--
--   * @H5D_CONTIGUOUS@: Store raw data separately from the object
--     header in one large chunk in the file.
--
--   * @H5D_CHUNKED@: Store raw data separately from the object header
--     as chunks of data in separate locations in the file.
--
--   * @H5D_VIRTUAL@: Draw raw data from multiple datasets in different files.
--
--   Note that a compact storage layout may affect writing data to the
--   dataset with parallel applications. See the note in @H5Dwrite@
--   documentation for details.
foreign import capi "hdf5-hs.h hs_H5Pset_layout" h5p_set_layout
  :: HID       -- ^ @plist_id@ Dataset creation property list identifier
  -> H5DLayout -- ^ @layout@ Type of storage layout for raw data
  -> HIO HErr


foreign import capi "hdf5-hs.h hs_H5Pset_chunk" h5p_set_chunk
  :: HID       -- ^ @plist_id@ Dataset creation property list identifier
  -> CInt      -- ^ @ndims@ The number of dimensions of each chunk
  -> Ptr HSize -- ^ @dim@ An array defining the size, in dataset elements, of each chunk
  -> HIO HErr  -- ^ Returns a non-negative value if successful;
               --   otherwise returns a negative value.

-- | @H5Pset_chunk_opts@ sets the edge chunk option in the dataset
--   creation property list @dcpl_id@. Valid values of flags are:
--
--   * @H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS@ When enabled, filters
--     are not applied to partial edge chunks. When disabled, partial
--     edge chunks are filtered. Enabling this option will improve
--     performance when appending to the dataset and, when compression
--     filters are used, prevent reallocation of these
--     chunks. Datasets created with this option enabled will be
--     inaccessible with HDF5 library versions before Release
--     1.10. Default: Disabled
--
--   * 0 (zero) Disables option; partial edge chunks will be compressed.
--
--   Motivation: @H5Pset_chunk_opts@ is used to specify storage
--   options for chunks on the edge of a dataset’s dataspace. This
--   capability allows the user to tune performance in cases where the
--   dataset size may not be a multiple of the chunk size and the
--   handling of partial edge chunks can impact performance.
foreign import capi "hdf5-hs.h hs_H5Pset_chunk_opts" h5p_set_chunk_opts
  :: HID   -- ^ @plist_id@ Dataset creation property list identifier
  -> CUInt -- ^ @opts@ Edge chunk option flag.
  -> HIO HErr


-- | Sets deflate (GNU gzip) compression method and compression level.
foreign import capi "hdf5-hs.h hs_H5Pset_deflate" h5p_set_deflate
  :: HID    -- ^ @plist_id@ Dataset creation property list identifier
  -> CUInt  -- ^ @level@ compression level
  -> HIO HErr
