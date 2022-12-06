{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Datatypes API for HDF5.
module HDF5.C.T
  ( -- * Constants
    h5t_NATIVE_CHAR
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
    -- * Functions
  , h5t_get_size
  ) where

import Foreign.C
import HDF5.C.Types


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


----------------------------------------------------------------
-- Functions
----------------------------------------------------------------


-- | @H5Tget_size@ returns the size of a datatype in bytes.
--
--   * For atomic datatypes, array datatypes, compound datatypes, and
--     other datatypes of a constant size, the returned value is the
--     size of the actual datatype in bytes.
--
--   * For variable-length
--     string datatypes the returned value is the size of the pointer
--     to the actual string, or @sizeof(char *)@. This function does not
--     return the size of actual variable-length string data.
--
--   * For variable-length sequence datatypes (see 'h5t_vlen_create'),
--     the returned value is the size of the @hvl_t struct@, or
--     @sizeof(hvl_t)@. The hvl_t struct contains a pointer to the
--     actual data and a size value. This function does not return the
--     size of actual variable-length sequence data.
foreign import capi "hdf5.h H5Tget_size" h5t_get_size
  :: HID      -- ^ Datatype identifier
  -> IO CSize
{-

hid_t       H5Tcreate (H5T_class_t type, size_t size)
hid_t       H5Tcopy (hid_t type_id)
herr_t      H5Tclose (hid_t type_id)
herr_t      H5Tclose_async (hid_t type_id, hid_t es_id)
htri_t      H5Tequal (hid_t type1_id, hid_t type2_id)
herr_t      H5Tlock (hid_t type_id)
herr_t      H5Tcommit2 (hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_i
herr_t      H5Tcommit_async (hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t es_id)
hid_t       H5Topen2 (hid_t loc_id, const char *name, hid_t tapl_id)
hid_t       H5Topen_async (hid_t loc_id, const char *name, hid_t tapl_id, hid_t es_id)
herr_t      H5Tcommit_anon (hid_t loc_id, hid_t type_id, hid_t tcpl_id, hid_t tapl_id)
hid_t       H5Tget_create_plist (hid_t type_id)
htri_t      H5Tcommitted (hid_t type_id)
herr_t      H5Tencode (hid_t obj_id, void *buf, size_t *nalloc)
hid_t       H5Tdecode (const void *buf)
herr_t      H5Tflush (hid_t type_id)
herr_t      H5Trefresh (hid_t type_id)
hid_t       H5Tget_super (hid_t type)
H5T_class_t H5Tget_class (hid_t type_id)
htri_t      H5Tdetect_class (hid_t type_id, H5T_class_t cls)
hid_t       H5Tget_native_type (hid_t type_id, H5T_direction_t direction)
herr_t      H5Tset_size (hid_t type_id, size_t size)
herr_t      H5Tcommit1 (hid_t loc_id, const char *name, hid_t type_id)
hid_t       H5Topen1 (hid_t loc_id, const char *name)
-}
