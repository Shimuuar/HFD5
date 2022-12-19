{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Attributes API
module HDF5.C.H5A
  ( h5a_open
  , h5a_close
  , h5a_read
  , h5a_write
  , h5a_exists
  , h5a_get_type
  , h5a_get_space
  ) where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types


-- | This function, @H5Aopen_by_idx@ or @H5Aopen_by_name@ must be
--   called before the attribute can be accessed for any further
--   purpose, including reading, writing, or any modification.
--
--   The attribute identifier returned by this function must be
--   released with @H5Aclose@ or resource leaks will develop.
--
--   Returns an attribute identifier if successful; otherwise returns
--   a negative value.
foreign import capi "hdf5.h H5Aopen" h5a_open
  :: HID     -- ^ @obj_id@ Location identifier. The identifier may be
             --   that of a file, group, dataset, or named datatype.
  -> CString -- ^ Name of attribute to open
  -> HID     -- ^ Attribute access property list identifier. parameter
             --   is currently not used; specify H5P_DEFAULT.
  -> IO HID

-- | Closes the specified attribute.
foreign import capi "hdf5.h H5Aclose" h5a_close
  :: HID     -- ^ Attribute identifier
  -> IO HErr



-- | @H5Aread@ reads an attribute, specified with attr_id. The
--   attribute's in-memory datatype is specified with type_id. The
--   entire attribute is read into buf from the file.
--
--   Datatype conversion takes place at the time of a read or write
--   and is automatic.
foreign import capi "hdf5.h H5Aread" h5a_read
  :: HID     -- ^ @attr_id@ Attribute identifier
  -> HID     -- ^ @type_id@ Datatype (in-memory) identifier
  -> Ptr ()  -- ^ @buf@     Buffer for data to be read
  -> IO HErr -- ^ Returns a non-negative value if successful;
             --   otherwise returns a negative value.

-- | @H5Awrite@ writes an attribute, specified with @attr_id@. The
--   attribute's in-memory datatype is specified with @type_id@. The
--   entire attribute is written from buf to the file.
--
--   Datatype conversion takes place at the time of a read or write
--   and is automatic.
foreign import capi "hdf5.h H5Awrite" h5a_write
  :: HID     -- ^ @attr_id@ Attribute identifier
  -> HID     -- ^ @type_id@ Datatype (in-memory) identifier
  -> Ptr ()  -- ^ @buf@ Data to be written
  -> IO HErr -- ^ Returns a non-negative value if successful;
             --   otherwise returns a negative

-- | Determines whether an attribute with a given name exists on an object.
foreign import capi "hdf5.h H5Aexists" h5a_exists
  :: HID     -- ^ @obj_id@ Location identifier. The identifier may be
             --   that of a file, group, dataset, or named datatype.
  -> CString -- ^ @attr_name@ Attribute name
  -> IO HTri -- ^ Returns zero (false), a positive (true) or a negative (failure) value.

-- | @H5Aget_type@ retrieves a copy of the attribute's datatype. The
--   datatype is reopened if it is a named type before returning it to
--   the application. The datatypes returned by this function are
--   always read-only.
foreign import capi "hdf5.h H5Aget_type" h5a_get_type
  :: HID    -- ^ @attr_id@ Attribute identifier
  -> IO HID

-- | @H5Aget_space@ retrieves a copy of the dataspace for an
--   attribute. The dataspace identifier returned from this function
--   must be released with @H5Sclose@ or resource leaks will develop.
foreign import capi "hdf5.h H5Aget_space" h5a_get_space
  :: HID    -- ^ @attr_id@ Attribute identifier
  -> IO HID -- ^ Returns an attribute dataspace identifier if
            --   successful; otherwise returns a negative value.

{-
hid_t 	H5Acreate2 (hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id)
hid_t 	H5Acreate_by_name (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t lapl_id)
herr_t 	H5Adelete (hid_t loc_id, const char *attr_name)
herr_t 	H5Adelete_by_idx (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t lapl_id)
herr_t 	H5Adelete_by_name (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t lapl_id)

htri_t 	H5Aexists_by_name (hid_t obj_id, const char *obj_name, const char *attr_name, hid_t lapl_id)
hid_t 	H5Aget_create_plist (hid_t attr_id)
herr_t 	H5Aget_info (hid_t attr_id, H5A_info_t *ainfo)
herr_t 	H5Aget_info_by_idx (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5A_info_t *ainfo, hid_t lapl_id)
herr_t 	H5Aget_info_by_name (hid_t loc_id, const char *obj_name, const char *attr_name, H5A_info_t *ainfo, hid_t lapl_id)
ssize_t H5Aget_name (hid_t attr_id, size_t buf_size, char *buf)
ssize_t H5Aget_name_by_idx (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, char *name, size_t size, hid_t lapl_id)
hid_t 	H5Aget_space (hid_t attr_id)
hsize_t H5Aget_storage_size (hid_t attr_id)
hid_t 	H5Aget_type (hid_t attr_id)
herr_t 	H5Aiterate2 (hid_t loc_id, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op, void *op_data)
herr_t 	H5Aiterate_by_name (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op, void *op_data, hid_t lapl_id)
hid_t 	H5Aopen_by_name (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id, hid_t lapl_id)
hid_t 	H5Aopen_by_idx (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, hid_t aapl_id, hid_t lapl_id)
herr_t 	H5Arename (hid_t loc_id, const char *old_name, const char *new_name)
herr_t 	H5Arename_by_name (hid_t loc_id, const char *obj_name, const char *old_attr_name, const char *new_attr_name, hid_t lapl_id)
hid_t 	H5Acreate1 (hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t acpl_id)
int 	H5Aget_num_attrs (hid_t loc_id)
herr_t 	H5Aiterate1 (hid_t loc_id, unsigned *idx, H5A_operator1_t op, void *op_data)
hid_t 	H5Aopen_idx (hid_t loc_id, unsigned idx)
hid_t 	H5Aopen_name (hid_t loc_id, const char *name)
-}
