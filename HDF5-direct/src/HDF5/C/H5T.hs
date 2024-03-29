
{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
-- Datatypes API for HDF5.
module HDF5.C.H5T
  ( -- * Enumeration
    -- ** Classes of types
    H5TClass(..)
  , pattern H5T_NO_CLASS
  , pattern H5T_INTEGER
  , pattern H5T_FLOAT
  , pattern H5T_TIME
  , pattern H5T_STRING
  , pattern H5T_BITFIELD
  , pattern H5T_OPAQUE
  , pattern H5T_COMPOUND
  , pattern H5T_REFERENCE
  , pattern H5T_ENUM
  , pattern H5T_VLEN
  , pattern H5T_ARRAY
  , pattern H5T_NCLASSES
    -- ** Byte order
  , H5TOrder(..)
  , pattern H5T_ORDER_ERROR
  , pattern H5T_ORDER_LE
  , pattern H5T_ORDER_BE
  , pattern H5T_ORDER_VAX
  , pattern H5T_ORDER_MIXED
  , pattern H5T_ORDER_NONE
    -- ** Sign information
  , H5TSign(..)
  , pattern H5T_SGN_ERROR
  , pattern H5T_SGN_NONE
  , pattern H5T_SGN_2
  , pattern H5T_NSGN
    -- * Constants
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
  , h5t_STD_I8BE
  , h5t_STD_I8LE
  , h5t_STD_I16BE
  , h5t_STD_I16LE
  , h5t_STD_I32BE
  , h5t_STD_I32LE
  , h5t_STD_I64BE
  , h5t_STD_I64LE
  , h5t_STD_U8BE
  , h5t_STD_U8LE
  , h5t_STD_U16BE
  , h5t_STD_U16LE
  , h5t_STD_U32BE
  , h5t_STD_U32LE
  , h5t_STD_U64BE
  , h5t_STD_U64LE
    -- * Functions
  , h5t_close
  , h5t_create
  , h5t_get_class
  , h5t_get_size
    -- ** Atomic types
  , h5t_get_order
  , h5t_get_precision
  , h5t_get_sign
  , h5t_get_super
    -- ** Array types
  , h5t_array_create
  , h5t_get_array_ndims
  , h5t_get_array_dims
    -- ** Enumerations and compound types
  , h5t_get_nmembers
  , h5t_get_member_name
  , h5t_get_member_index
    -- *** Compound types
  , h5t_insert
  , h5t_get_member_type
    -- *** Enumerations
  , h5t_enum_create
  , h5t_enum_insert
  , h5t_enum_nameof
  , h5t_enum_valueof
  , h5t_get_member_value
  ) where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types

----------------------------------------------------------------
-- Enumerations
----------------------------------------------------------------

newtype H5TClass = H5TClass CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5T_NO_CLASS"  h5t_NO_CLASS  :: H5TClass
foreign import capi "hdf5.h value H5T_INTEGER"   h5t_INTEGER   :: H5TClass
foreign import capi "hdf5.h value H5T_FLOAT"     h5t_FLOAT     :: H5TClass
foreign import capi "hdf5.h value H5T_TIME"      h5t_TIME      :: H5TClass
foreign import capi "hdf5.h value H5T_STRING"    h5t_STRING    :: H5TClass
foreign import capi "hdf5.h value H5T_BITFIELD"  h5t_BITFIELD  :: H5TClass
foreign import capi "hdf5.h value H5T_OPAQUE"    h5t_OPAQUE    :: H5TClass
foreign import capi "hdf5.h value H5T_COMPOUND"  h5t_COMPOUND  :: H5TClass
foreign import capi "hdf5.h value H5T_REFERENCE" h5t_REFERENCE :: H5TClass
foreign import capi "hdf5.h value H5T_ENUM"      h5t_ENUM      :: H5TClass
foreign import capi "hdf5.h value H5T_VLEN"      h5t_VLEN      :: H5TClass
foreign import capi "hdf5.h value H5T_ARRAY"     h5t_ARRAY     :: H5TClass
foreign import capi "hdf5.h value H5T_NCLASSES"  h5t_NCLASSES  :: H5TClass

pattern H5T_NO_CLASS :: H5TClass
pattern H5T_NO_CLASS <- ((==h5t_NO_CLASS) -> True) where H5T_NO_CLASS = h5t_NO_CLASS

pattern H5T_INTEGER :: H5TClass
pattern H5T_INTEGER <- ((==h5t_INTEGER) -> True) where H5T_INTEGER = h5t_INTEGER

pattern H5T_FLOAT :: H5TClass
pattern H5T_FLOAT <- ((==h5t_FLOAT) -> True) where H5T_FLOAT = h5t_FLOAT

pattern H5T_TIME :: H5TClass
pattern H5T_TIME <- ((==h5t_TIME) -> True) where H5T_TIME = h5t_TIME

pattern H5T_STRING :: H5TClass
pattern H5T_STRING <- ((==h5t_STRING) -> True) where H5T_STRING = h5t_STRING

pattern H5T_BITFIELD :: H5TClass
pattern H5T_BITFIELD <- ((==h5t_BITFIELD) -> True) where H5T_BITFIELD = h5t_BITFIELD

pattern H5T_OPAQUE :: H5TClass
pattern H5T_OPAQUE <- ((==h5t_OPAQUE) -> True) where H5T_OPAQUE = h5t_OPAQUE

pattern H5T_COMPOUND :: H5TClass
pattern H5T_COMPOUND <- ((==h5t_COMPOUND) -> True) where H5T_COMPOUND = h5t_COMPOUND

pattern H5T_REFERENCE :: H5TClass
pattern H5T_REFERENCE <- ((==h5t_REFERENCE) -> True) where H5T_REFERENCE = h5t_REFERENCE

pattern H5T_ENUM :: H5TClass
pattern H5T_ENUM <- ((==h5t_ENUM) -> True) where H5T_ENUM = h5t_ENUM

pattern H5T_VLEN :: H5TClass
pattern H5T_VLEN <- ((==h5t_VLEN) -> True) where H5T_VLEN = h5t_VLEN

pattern H5T_ARRAY :: H5TClass
pattern H5T_ARRAY <- ((==h5t_ARRAY) -> True) where H5T_ARRAY = h5t_ARRAY

pattern H5T_NCLASSES :: H5TClass
pattern H5T_NCLASSES <- ((==h5t_NCLASSES) -> True) where H5T_NCLASSES = h5t_NCLASSES


----------------------------------------

-- | Byte order for data type
newtype H5TOrder = H5TOrder CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5T_ORDER_ERROR" h5t_ORDER_ERROR :: H5TOrder
foreign import capi "hdf5.h value H5T_ORDER_LE"    h5t_ORDER_LE    :: H5TOrder
foreign import capi "hdf5.h value H5T_ORDER_BE"    h5t_ORDER_BE    :: H5TOrder
foreign import capi "hdf5.h value H5T_ORDER_VAX"   h5t_ORDER_VAX   :: H5TOrder
foreign import capi "hdf5.h value H5T_ORDER_MIXED" h5t_ORDER_MIXED :: H5TOrder
foreign import capi "hdf5.h value H5T_ORDER_NONE"  h5t_ORDER_NONE  :: H5TOrder

-- | Error
pattern H5T_ORDER_ERROR :: H5TOrder
pattern H5T_ORDER_ERROR <- ((==h5t_ORDER_ERROR) -> True) where H5T_ORDER_ERROR = h5t_ORDER_ERROR

-- | Little endian
pattern H5T_ORDER_LE :: H5TOrder
pattern H5T_ORDER_LE <- ((==h5t_ORDER_LE) -> True) where H5T_ORDER_LE = h5t_ORDER_LE

-- | Big endian
pattern H5T_ORDER_BE :: H5TOrder
pattern H5T_ORDER_BE <- ((==h5t_ORDER_BE) -> True) where H5T_ORDER_BE = h5t_ORDER_BE

-- | VAX mixed endian
pattern H5T_ORDER_VAX :: H5TOrder
pattern H5T_ORDER_VAX <- ((==h5t_ORDER_VAX) -> True) where H5T_ORDER_VAX = h5t_ORDER_VAX

-- | Compound type with mixed member orders
pattern H5T_ORDER_MIXED :: H5TOrder
pattern H5T_ORDER_MIXED <- ((==h5t_ORDER_MIXED) -> True) where H5T_ORDER_MIXED = h5t_ORDER_MIXED

-- | no particular order (strings, bits,..)
pattern H5T_ORDER_NONE :: H5TOrder
pattern H5T_ORDER_NONE <- ((==h5t_ORDER_NONE) -> True) where H5T_ORDER_NONE = h5t_ORDER_NONE

----------------------------------------

-- | Byte order for data type
newtype H5TSign = H5TSign CInt
  deriving (Show,Eq,Ord)


foreign import capi "hdf5.h value H5T_SGN_ERROR" h5t_SGN_ERROR :: H5TSign
foreign import capi "hdf5.h value H5T_SGN_NONE"  h5t_SGN_NONE  :: H5TSign
foreign import capi "hdf5.h value H5T_SGN_2"     h5t_SGN_2     :: H5TSign
foreign import capi "hdf5.h value H5T_NSGN"      h5t_NSGN      :: H5TSign

-- | Error
pattern H5T_SGN_ERROR :: H5TSign
pattern H5T_SGN_ERROR <- ((==h5t_SGN_ERROR) -> True) where H5T_SGN_ERROR = h5t_SGN_ERROR

-- | this is an unsigned type
pattern H5T_SGN_NONE :: H5TSign
pattern H5T_SGN_NONE <- ((==h5t_SGN_NONE) -> True) where H5T_SGN_NONE = h5t_SGN_NONE

-- | Two's complement
pattern H5T_SGN_2 :: H5TSign
pattern H5T_SGN_2 <- ((==h5t_SGN_2) -> True) where H5T_SGN_2 = h5t_SGN_2

pattern H5T_NSGN :: H5TSign
pattern H5T_NSGN <- ((==h5t_NSGN) -> True) where H5T_NSGN = h5t_NSGN



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



foreign import capi "hdf5.h value H5T_STD_I8BE"  h5t_STD_I8BE  :: HID
foreign import capi "hdf5.h value H5T_STD_I8LE"  h5t_STD_I8LE  :: HID
foreign import capi "hdf5.h value H5T_STD_I16BE" h5t_STD_I16BE :: HID
foreign import capi "hdf5.h value H5T_STD_I16LE" h5t_STD_I16LE :: HID
foreign import capi "hdf5.h value H5T_STD_I32BE" h5t_STD_I32BE :: HID
foreign import capi "hdf5.h value H5T_STD_I32LE" h5t_STD_I32LE :: HID
foreign import capi "hdf5.h value H5T_STD_I64BE" h5t_STD_I64BE :: HID
foreign import capi "hdf5.h value H5T_STD_I64LE" h5t_STD_I64LE :: HID
foreign import capi "hdf5.h value H5T_STD_U8BE"  h5t_STD_U8BE  :: HID
foreign import capi "hdf5.h value H5T_STD_U8LE"  h5t_STD_U8LE  :: HID
foreign import capi "hdf5.h value H5T_STD_U16BE" h5t_STD_U16BE :: HID
foreign import capi "hdf5.h value H5T_STD_U16LE" h5t_STD_U16LE :: HID
foreign import capi "hdf5.h value H5T_STD_U32BE" h5t_STD_U32BE :: HID
foreign import capi "hdf5.h value H5T_STD_U32LE" h5t_STD_U32LE :: HID
foreign import capi "hdf5.h value H5T_STD_U64BE" h5t_STD_U64BE :: HID
foreign import capi "hdf5.h value H5T_STD_U64LE" h5t_STD_U64LE :: HID


----------------------------------------------------------------
-- Functions
----------------------------------------------------------------


-- | @H5Tclose@ releases the datatype dtype_id. Further access through
--   this datatype identifier is illegal. Failure to release a
--   datatype with this call will result in resource leaks.
--
--   Returns a non-negative value if successful; otherwise returns a
--   negative value.
foreign import capi "hdf5-hs.h hs_H5Tclose" h5t_close
  :: HID     -- ^ Datatype identifier
  -> HIO HErr


-- | @H5Tcreate@ creates a new datatype of the specified class with
--   the specified number of bytes. This function is used only with the
--   following datatype classes:
--
-- > H5T_COMPOUND
-- > H5T_OPAQUE
-- > H5T_ENUM
-- > H5T_STRING
--
-- Other datatypes, including integer and floating-point datatypes,
-- are typically created by using @H5Tcopy@ to copy and modify a
-- predefined datatype.
--
-- When creating a variable-length string datatype, size must be
-- @H5T_VARIABLE@; see Creating variable-length string datatypes.
--
-- When creating a fixed-length string datatype, size will be the
-- length of the string in bytes. The length of the string in
-- characters will depend on i the encoding used; see
-- H5Pset_char_encoding.
--
-- ENUMs created with this function have a signed native integer base
-- datatype. Use @H5Tenum_create@ if a different integer base datatype
-- is required.
--
-- The datatype identifier returned from this function should be
-- released with H5Tclose or resource leaks will result.
foreign import capi "hdf5-hs.h hs_H5Tcreate" h5t_create
  :: H5TClass -- ^ @type@ Class of datatype to create
  -> CSize    -- ^ @size@ Size, in bytes, of the datatype being created
  -> HIO HID  -- ^ Returns a datatype identifier if successful;
              --   otherwise returns H5I_INVALID_HID.

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
foreign import capi "hdf5-hs.h hs_H5Tget_size" h5t_get_size
  :: HID       -- ^ Datatype identifier
  -> HIO CSize -- ^ Returns the size of the datatype in bytes if
               --   successful; otherwise, returns 0.

-- | @H5Tget_class@ returns the class of the datatype type_id.
--
--   Returns the datatype class if successful; otherwise @H5T_NO_CLASS@.
foreign import capi "hdf5-hs.h hs_H5Tget_class" h5t_get_class
  :: HID -- ^ Datatype identifier
  -> HIO H5TClass


-- | Returns the byte order of an atomic datatype.
--
--    Members of a compound datatype need not have the same byte
--    order. If members of a compound datatype have more than one of
--    little endian, big endian, or VAX byte order, H5Tget_order()
--    will return H5T_ORDER_MIXED for the compound datatype. A byte
--    order of H5T_ORDER_NONE will, however, be ignored; for example,
--    if one or more members of a compound datatype have byte order
--    H5T_ORDER_NONE but all other members have byte order
--    H5T_ORDER_LE, H5Tget_order() will return H5T_ORDER_LE for the
--    compound datatype.
foreign import capi "hdf5-hs.h hs_H5Tget_order" h5t_get_order
  :: HID -- ^ Datatype identifier
  -> HIO H5TOrder


-- | Returns the precision of an atomic datatype (for example, integer
--   or float) or a datatype whose base (parent) type is an atomic
--   type (for example, array, enum and variable length). The
--   precision is the number of significant bits which, unless padding
--   is present, is 8 times larger than the value returned by
--   H5Tget_size().
--
--   Returns the number of significant bits if successful; otherwise 0
foreign import capi "hdf5-hs.h hs_H5Tget_precision" h5t_get_precision
  :: HID        -- ^ Datatype identifier
  -> HIO CSize

foreign import capi "hdf5-hs.h hs_H5Tget_sign" h5t_get_sign
  :: HID        -- ^ Datatype identifier
  -> HIO H5TSign

-- | @H5Tget_super@ returns the base datatype from which the datatype
--   type_id is derived. In the case of an enumeration type, the
--   return value is an integer type.
--
--   The datatype identifier returned by this function must be
--   released with @H5Tclose@ when the identifier is no longer needed
--   so that resource leaks will not develop.
--
--   Returns a datatype identifier if successful; otherwise returns
--   @H5I_INVALID_HID@.
foreign import capi "hdf5-hs.h hs_H5Tget_super" h5t_get_super
  :: HID     -- ^ Datatype identifier
  -> HIO HID

-- | @H5Tarray_create2@ creates a new array datatype object.
--
--   @base_id@ is the datatype of every element of the array, i.e., of
--   the number at each position in the array.
--
--   @ndims@ is the number of dimensions and the size of each
--   dimension is specified in the array dim. The value of rank is
--   currently limited to @H5S_MAX_RANK@ and must be greater than 0
--   (zero). All dimension sizes specified in dim must be greater than
--   0 (zero).
--
--   Returns a array datatype identifier if successful; otherwise
--   returns @H5I_INVALID_HID@.
foreign import capi "hdf5-hs.h hs_H5Tarray_create2" h5t_array_create
  :: HID       -- ^ @base_id@
  -> CUInt     -- ^ @ndims@
  -> Ptr HSize -- ^ @dim@
  -> HIO HID

-- | Returns the non-negative number of dimensions of the array type
--   if successful; otherwise returns a negative value.
foreign import capi "hdf5-hs.h hs_H5Tget_array_ndims" h5t_get_array_ndims
  :: HID     -- ^ Type ID
  -> HIO CInt

-- | @H5Tget_array_dims2@ returns the sizes of the dimensions of the
--   specified array datatype object in the array dims.
--
--   Returns the non-negative number of dimensions of the array type
--   if successful; otherwise returns a negative value.
foreign import capi "hdf5-hs.h hs_H5Tget_array_dims2" h5t_get_array_dims
  :: HID       -- ^ Type ID
  -> Ptr HSize -- ^ @[out]@ Sizes of array dimensions
  -> HIO CInt

-- | Retrieves the number of elements in a compound or enumeration
--   datatype.
foreign import capi "hdf5-hs.h hs_H5Tget_nmembers" h5t_get_nmembers
  :: HID      -- ^ Datatype identifier
  -> HIO CInt -- ^ Returns the number of elements if successful;
              --   otherwise returns a negative value.

-- | @H5Tget_member_name@ retrieves the name of a field of a compound
--   datatype or an element of an enumeration datatype.
--
--   The index of the target field or element is specified in
--   @member_no@. Compound datatype fields and enumeration datatype
--   elements are stored in no particular order with index values of 0
--   through @N-1@, where @N@ is the value returned by @H5Tget_nmembers@.
--
--   The HDF5 library allocates a buffer to receive the name of the
--   field. The caller must subsequently free the buffer with
--   @H5free_memory@.
foreign import capi "hdf5-hs.h hs_H5Tget_member_name" h5t_get_member_name
  :: HID         -- ^ Datatype identifier
  -> CUInt       -- ^ @membno@ Zero-based index of the field or element
  -> HIO CString -- ^ Returns a valid pointer to a string allocated
                 --   with malloc() if successful; otherwise returns
                 --   NULL.

-- | @H5Tget_member_index@ retrieves the index of a field of a
--   compound datatype or an element of an enumeration datatype.
--
--   The name of the target field or element is specified by name.
--
--   Fields are stored in no particular order with index values of 0
--   through @N-1@, where @N@ is the value returned by @H5Tget_nmembers@.
foreign import capi "hdf5-hs.h hs_H5Tget_member_index" h5t_get_member_index
  :: HID      -- ^ @type_id@ Datatype identifier
  -> CString  -- ^ @name@ Name of the field or member
  -> HIO CInt -- ^ Returns a non-negative value if successful;
              --   otherwise returns a negative value.


-- ^ @H5Tinsert@ adds another member to the compound datatype,
--   specified @type_id@.
--
--   The new member has a name which must be unique within the
--   compound datatype. The offset argument defines the start of the
--   member in an instance of the compound datatype, and @member_id@
--   is the datatype identifier of the new member.
foreign import capi "hdf5-hs.h hs_H5Tinsert" h5t_insert
  :: HID      -- ^ @parent_id@ Datatype identifier
  -> CString  -- ^ @name@ Name of the field to insert
  -> CSize    -- ^ @offset@ Offset in memory structure of the field to insert
  -> HID      -- ^ @member_id@ Datatype identifier of the field to insert
  -> HIO HErr -- ^ Returns a non-negative value if successful;
              --   otherwise returns a negative value.


-- | Returns the datatype of the specified member.
foreign import capi "hdf5-hs.h hs_H5Tget_member_type" h5t_get_member_type
  :: HID     -- ^ @type_id@ Datatype identifier
  -> CUInt   -- ^ @membno@ Zero-based index of the field or element
  -> HIO HID -- ^ Returns the identifier of a copy of the datatype of
             --   the field if successful; otherwise returns a
             --   negative value.


-- | @H5Tenum_create@ creates a new enumeration datatype based on the
--   specified base datatype, dtype_id, which must be an integer
--   datatype.
--
--   If a particular architecture datatype is required, a little
--   endian or big endian datatype for example, use a native datatype
--   as the base datatype and use @H5Tconvert@ on values as they are
--   read from or written to a dataset.
foreign import capi "hdf5-hs.h hs_H5Tenum_create" h5t_enum_create
  :: HID     -- ^ @base_id@ Datatype identifier for the base
             --   datatype. Must be an integer datatype
  -> HIO HID -- ^ Returns a enumeration datatype identifier if
             --   successful; otherwise returns H5I_INVALID_HID.


-- | @H5Tenum_insert@ inserts a new enumeration datatype member into
--   an enumeration datatype.
--
--   @type_id@ is the datatype identifier for the enumeration
--   datatype, name is the name of the new member, and value points to
--   the value of the new member.
--
--   @name@ and @value@ must both be unique within dtype_id.
--
--   @value@ points to data which must be of the integer base datatype
--   used when the enumeration datatype was created. If a particular
--   architecture datatype is required, a little endian or big endian
--   datatype for example, use a native datatype as the base datatype
--   and use @H5Tconvert@ on values as they are read from or written
--   to a dataset.
foreign import capi "hdf5-hs.h hs_H5Tenum_insert" h5t_enum_insert
  :: HID      -- ^ @type@ Datatype identifier
  -> CString  -- ^ @name@ Name of the new member
  -> Ptr x    -- ^ @value@ Pointer to the value of the new member
  -> HIO HErr -- ^ Returns a non-negative value if successful;
              --   otherwise returns a negative value.

-- | @H5Tenum_nameof@ finds the symbol name that corresponds to the
--   specified value of the enumeration datatype type.
--
--   At most size characters of the symbol name are copied into the
--   name buffer. If the entire symbol name and null terminator do not
--   fit in the name buffer, then as many characters as possible are
--   copied (not null terminated) and the function fails.
foreign import capi "hdf5-hs.h hs_H5Tenum_nameof" h5t_enum_nameof
  :: HID      -- ^ @type@ Datatype identifier
  -> Ptr x    -- ^ @value@ Value of the enumeration datatype
  -> CString  -- ^ @[out]@ @name@ Buffer for output of the symbol name
  -> CSize    -- ^ @size@ Anticipated size of the symbol name, in bytes
  -> HIO HErr -- ^ Returns a non-negative value if
              --   successful. Otherwise returns a negative value


-- | @H5Tenum_valueof@ finds the value that corresponds to the
--   specified name of the enumeration datatype dtype_id.
--
--   Values returned in value will be of the enumerated type’s base
--   type, that is, the datatype used by @H5Tenum_create@ when the
--   enumerated type was created.
--
--   The value buffer must be at least large enough to hold a value of
--   that base type. If the size is unknown, you can determine it with
--   @H5Tget_size@.
foreign import capi "hdf5-hs.h hs_H5Tenum_valueof" h5t_enum_valueof
  :: HID      -- ^ @type@ Datatype identifier
  -> CString  -- ^ @name@ Symbol name of the enumeration datatype
  -> Ptr x    -- ^ @[out]@ @value@ Buffer for the value of the
              --   enumeration datatype
  -> HIO HErr -- ^ Returns a non-negative value if successful;
              --   otherwise returns a negative value.


-- | @H5Tget_member_value@ returns the value of the enumeration
--   datatype member member_no.
--
--   The member value is returned in a user-supplied buffer pointed to
--   by value. Values returned in value will be of the enumerated
--   type’s base type, that is, the datatype used by @H5Tenum_create@
--   when the enumerated type was created.
--
--   The value buffer must be at least large enough to hold a value of
--   that base type. If the size is unknown, you can determine it with
--   @H5Tget_size@.
foreign import capi "hdf5-hs.h hs_H5Tget_member_value" h5t_get_member_value
  :: HID      -- ^ @type_id@ Datatype identifier
  -> CUInt    -- ^ @membno@ Number of the enumeration datatype member
  -> Ptr x    -- ^ @[out]@ @value@ Buffer for the value of the enumeration datatype member
  -> HIO HErr -- ^ Returns a non-negative value if successful;
              --   otherwise returns a negative value.

{-
hid_t       H5Tcopy (hid_t type_id)
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
htri_t      H5Tdetect_class (hid_t type_id, H5T_class_t cls)
hid_t       H5Tget_native_type (hid_t type_id, H5T_direction_t direction)
herr_t      H5Tset_size (hid_t type_id, size_t size)
herr_t      H5Tcommit1 (hid_t loc_id, const char *name, hid_t type_id)
hid_t       H5Topen1 (hid_t loc_id, const char *name)
-}



{- ATOMIC
int         H5Tget_offset (hid_t type_id)
herr_t      H5Tget_pad (hid_t type_id, H5T_pad_t *lsb, H5T_pad_t *msb)
herr_t      H5Tget_fields (hid_t type_id, size_t *spos, size_t *epos, size_t *esize, size_t *mpos, size_t *msize)
size_t      H5Tget_ebias (hid_t type_id)
H5T_norm_t  H5Tget_norm (hid_t type_id)
H5T_pad_t   H5Tget_inpad (hid_t type_id)
H5T_str_t   H5Tget_strpad (hid_t type_id)
H5T_cset_t  H5Tget_cset (hid_t type_id)
htri_t      H5Tis_variable_str (hid_t type_id)
herr_t      H5Tset_order (hid_t type_id, H5T_order_t order)
herr_t      H5Tset_precision (hid_t type_id, size_t prec)
herr_t      H5Tset_offset (hid_t type_id, size_t offset)
herr_t      H5Tset_pad (hid_t type_id, H5T_pad_t lsb, H5T_pad_t msb)
herr_t      H5Tset_sign (hid_t type_id, H5T_sign_t sign)
herr_t      H5Tset_fields (hid_t type_id, size_t spos, size_t epos, size_t esize, size_t mpos, size_t msize)
herr_t      H5Tset_ebias (hid_t type_id, size_t ebias)
herr_t      H5Tset_norm (hid_t type_id, H5T_norm_t norm)
herr_t      H5Tset_inpad (hid_t type_id, H5T_pad_t pad)
herr_t      H5Tset_cset (hid_t type_id, H5T_cset_t cset)
herr_t      H5Tset_strpad (hid_t type_id, H5T_str_t strpad)
-}


{- ENUM & COMPOUND
int     H5Tget_nmembers(hid_t type_id)
char *  H5Tget_member_name(hid_t type_id, unsigned membno)
int     H5Tget_member_index(hid_t type_id, const char *name)

herr_t  H5Tinsert(hid_t parent_id, const char *name, size_t offset, hid_t member_id)
herr_t  H5Tpack(hid_t type_id)
size_t  H5Tget_member_offset(hid_t type_id, unsigned membno)
H5T_class_t H5Tget_member_class(hid_t type_id, unsigned membno)
hid_t   H5Tget_member_type(hid_t type_id, unsigned membno)
-}
