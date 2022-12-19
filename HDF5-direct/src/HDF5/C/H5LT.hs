{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- High level function for API
module HDF5.C.H5LT
  ( -- * Enumerations
    H5LTLang(..)
  , h5lt_LANG_ERR
  , h5lt_DDL
  , h5lt_C
  , h5lt_FORTRAN
  , h5lt_NO_LANG
    -- * Functions
  , h5lt_dtype_to_text
  ) where


import Foreign.C
import Foreign.Ptr
import HDF5.C.Types


newtype H5LTLang = H5LTLang CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5_hl.h value H5LT_LANG_ERR" h5lt_LANG_ERR :: H5LTLang
foreign import capi "hdf5_hl.h value H5LT_DDL"      h5lt_DDL      :: H5LTLang
foreign import capi "hdf5_hl.h value H5LT_C"        h5lt_C        :: H5LTLang
foreign import capi "hdf5_hl.h value H5LT_FORTRAN"  h5lt_FORTRAN  :: H5LTLang
foreign import capi "hdf5_hl.h value H5LT_NO_LANG"  h5lt_NO_LANG  :: H5LTLang


-- | Given an HDF5 datatype identifier, this function creates a
--   description of this datatype in lang_type language format. A
--   preliminary @H5LTdtype_to_text@ call can be made to determine the
--   size of the buffer needed with a NULL passed in for str. This
--   value is returned as len. That value can then be assigned to len
--   for a second @H5Ttype_to_text@ call, which will retrieve the
--   actual text description for the datatype.
--
--   If len is not big enough for the description, the text
--   description will be truncated to fit in the buffer.
--
--   Currently only DDL (H5LT_DDL) is supported for lang_type. The
--   complete DDL definition of HDF5 data types can be found in the
--   last chapter of the HDF5 User's Guide.
foreign import capi "hdf5_hl.h H5LTdtype_to_text" h5lt_dtype_to_text
  :: HID
  -> CString   -- ^ Buffer for the text description of the datatype
  -> H5LTLang  -- ^ The language used to describe the datatype. The
               --   currently supported language is H5LT_DDL.
  -> Ptr CSize -- ^ The size of buffer needed to store the text description.
  -> IO HErr

{-
H5_HLDLL herr_t H5LTmake_dataset (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, hid_t type_id, const void *buffer)
H5_HLDLL herr_t H5LTmake_dataset_char (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const char *buffer)
H5_HLDLL herr_t H5LTmake_dataset_short (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const short *buffer)
H5_HLDLL herr_t H5LTmake_dataset_int (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const int *buffer)
H5_HLDLL herr_t H5LTmake_dataset_long (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const long *buffer)
H5_HLDLL herr_t H5LTmake_dataset_float (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const float *buffer)
H5_HLDLL herr_t H5LTmake_dataset_double (hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const double *buffer)
H5_HLDLL herr_t H5LTmake_dataset_string (hid_t loc_id, const char *dset_name, const char *buf)
H5_HLDLL herr_t H5LTread_dataset (hid_t loc_id, const char *dset_name, hid_t type_id, void *buffer)
H5_HLDLL herr_t H5LTread_dataset_char (hid_t loc_id, const char *dset_name, char *buffer)
H5_HLDLL herr_t H5LTread_dataset_short (hid_t loc_id, const char *dset_name, short *buffer)
H5_HLDLL herr_t H5LTread_dataset_int (hid_t loc_id, const char *dset_name, int *buffer)
H5_HLDLL herr_t H5LTread_dataset_long (hid_t loc_id, const char *dset_name, long *buffer)
H5_HLDLL herr_t H5LTread_dataset_float (hid_t loc_id, const char *dset_name, float *buffer)
H5_HLDLL herr_t H5LTread_dataset_double (hid_t loc_id, const char *dset_name, double *buffer)
H5_HLDLL herr_t H5LTread_dataset_string (hid_t loc_id, const char *dset_name, char *buf)
H5_HLDLL herr_t H5LTget_dataset_ndims (hid_t loc_id, const char *dset_name, int *rank)
H5_HLDLL herr_t H5LTget_dataset_info (hid_t loc_id, const char *dset_name, hsize_t *dims, H5T_class_t *type_class, size_t *type_size)
H5_HLDLL herr_t H5LTfind_dataset (hid_t loc_id, const char *name)
H5_HLDLL herr_t H5LTset_attribute_string (hid_t loc_id, const char *obj_name, const char *attr_name, const char *attr_data)
H5_HLDLL herr_t H5LTset_attribute_char (hid_t loc_id, const char *obj_name, const char *attr_name, const char *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_uchar (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned char *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_short (hid_t loc_id, const char *obj_name, const char *attr_name, const short *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_ushort (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned short *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_int (hid_t loc_id, const char *obj_name, const char *attr_name, const int *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_uint (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned int *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_long (hid_t loc_id, const char *obj_name, const char *attr_name, const long *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_long_long (hid_t loc_id, const char *obj_name, const char *attr_name, const long long *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_ulong (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned long *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_ullong (hid_t loc_id, const char *obj_name, const char *attr_name, const unsigned long long *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_float (hid_t loc_id, const char *obj_name, const char *attr_name, const float *buffer, size_t size)
H5_HLDLL herr_t H5LTset_attribute_double (hid_t loc_id, const char *obj_name, const char *attr_name, const double *buffer, size_t size)
H5_HLDLL herr_t H5LTget_attribute (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t mem_type_id, void *data)
H5_HLDLL herr_t H5LTget_attribute_string (hid_t loc_id, const char *obj_name, const char *attr_name, char *data)
H5_HLDLL herr_t H5LTget_attribute_char (hid_t loc_id, const char *obj_name, const char *attr_name, char *data)
H5_HLDLL herr_t H5LTget_attribute_uchar (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned char *data)
H5_HLDLL herr_t H5LTget_attribute_short (hid_t loc_id, const char *obj_name, const char *attr_name, short *data)
H5_HLDLL herr_t H5LTget_attribute_ushort (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned short *data)
H5_HLDLL herr_t H5LTget_attribute_int (hid_t loc_id, const char *obj_name, const char *attr_name, int *data)
H5_HLDLL herr_t H5LTget_attribute_uint (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned int *data)
H5_HLDLL herr_t H5LTget_attribute_long (hid_t loc_id, const char *obj_name, const char *attr_name, long *data)
H5_HLDLL herr_t H5LTget_attribute_long_long (hid_t loc_id, const char *obj_name, const char *attr_name, long long *data)
H5_HLDLL herr_t H5LTget_attribute_ulong (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned long *data)
H5_HLDLL herr_t H5LTget_attribute_ullong (hid_t loc_id, const char *obj_name, const char *attr_name, unsigned long long *data)
H5_HLDLL herr_t H5LTget_attribute_float (hid_t loc_id, const char *obj_name, const char *attr_name, float *data)
H5_HLDLL herr_t H5LTget_attribute_double (hid_t loc_id, const char *obj_name, const char *attr_name, double *data)
H5_HLDLL herr_t H5LTget_attribute_ndims (hid_t loc_id, const char *obj_name, const char *attr_name, int *rank)
H5_HLDLL herr_t H5LTget_attribute_info (hid_t loc_id, const char *obj_name, const char *attr_name, hsize_t *dims, H5T_class_t *type_class, size_t *type_size)
H5_HLDLL hid_t 	H5LTtext_to_dtype (const char *text, H5LT_lang_t lang_type)
H5_HLDLL herr_t H5LTfind_attribute (hid_t loc_id, const char *name)
H5_HLDLL htri_t H5LTpath_valid (hid_t loc_id, const char *path, hbool_t check_object_valid)
H5_HLDLL hid_t 	H5LTopen_file_image (void *buf_ptr, size_t buf_size, unsigned flags)
-}
