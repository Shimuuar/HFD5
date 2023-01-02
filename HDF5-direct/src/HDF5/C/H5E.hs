{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
-- API for error handling
module HDF5.C.H5E
  ( -- * Constants and types
    h5e_DEFAULT
  , H5EDirection(..)
  , pattern H5E_WALK_UPWARD
  , pattern H5E_WALK_DOWNWARD
  , H5EType(..)
  , pattern H5E_MAJOR
  , pattern H5E_MINOR
    -- ** struct H5EError2
  , H5EError
  , h5e_error_cls_id
  , h5e_error_maj_num
  , h5e_error_min_num
  , h5e_error_line
  , h5e_error_func_name
  , h5e_error_file_name
  , h5e_error_desc
    -- ** Callback types
  , H5EAuto
  , H5EWalk
    -- * Functions
  , h5e_close_msg
  , h5e_close_stack
  , h5e_create_stack
  , h5e_get_current_stack
  , h5e_get_auto
  , h5e_set_auto
  , h5e_walk
  , h5e_get_msg

  , off_cls_id
  , off_maj_num
  , off_min_num
  , off_line
  , off_func_name
  , off_file_name
  , off_desc

  ) where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Default value to use with functions in this module
foreign import capi "hdf5.h value H5E_DEFAULT" h5e_DEFAULT :: HID


-- | Direction in which error stack is walked
newtype H5EDirection = H5EDirection CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5E_WALK_DOWNWARD" h5e_WALK_DOWNWARD :: H5EDirection
foreign import capi "hdf5.h value H5E_WALK_UPWARD"   h5e_WALK_UPWARD   :: H5EDirection

pattern H5E_WALK_UPWARD :: H5EDirection
pattern H5E_WALK_UPWARD <- ((==h5e_WALK_UPWARD) -> True) where H5E_WALK_UPWARD = h5e_WALK_UPWARD

pattern H5E_WALK_DOWNWARD :: H5EDirection
pattern H5E_WALK_DOWNWARD <- ((==h5e_WALK_DOWNWARD) -> True) where H5E_WALK_DOWNWARD = h5e_WALK_DOWNWARD


newtype H5EType = H5EType CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5E_MAJOR" h5e_MAJOR :: H5EType
foreign import capi "hdf5.h value H5E_MINOR" h5e_MINOR :: H5EType

pattern H5E_MAJOR :: H5EType
pattern H5E_MAJOR <- ((==h5e_MAJOR) -> True) where H5E_MAJOR = h5e_MAJOR

pattern H5E_MINOR :: H5EType
pattern H5E_MINOR <- ((==h5e_MINOR) -> True) where H5E_MINOR = h5e_MINOR


-- | Tag for struct @H5Eerror2_t@
--
-- > hid_t       cls_id
-- > hid_t       maj_num
-- > hid_t       min_num
-- > unsigned    line
-- > const char *func_name
-- > const char *file_name
-- > const char *desc
data H5EError

foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_cls_id"    off_cls_id    :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_maj_num"   off_maj_num   :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_min_num"   off_min_num   :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_line"      off_line      :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_func_name" off_func_name :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_file_name" off_file_name :: CSize
foreign import capi "hdf5-hs.h value hs_hdf5_off_error2_desc"      off_desc      :: CSize

h5e_error_cls_id :: Ptr H5EError -> Ptr HID
h5e_error_cls_id p = castPtr $ plusPtr p (fromIntegral off_cls_id)

h5e_error_maj_num :: Ptr H5EError -> Ptr HID
h5e_error_maj_num p = castPtr $ plusPtr p (fromIntegral off_maj_num)

h5e_error_min_num :: Ptr H5EError -> Ptr HID
h5e_error_min_num p = castPtr $ plusPtr p (fromIntegral off_min_num)

h5e_error_line :: Ptr H5EError -> Ptr CUInt
h5e_error_line p = castPtr $ plusPtr p (fromIntegral off_line)

h5e_error_func_name :: Ptr H5EError -> Ptr CString
h5e_error_func_name p = castPtr $ plusPtr p (fromIntegral off_func_name)

h5e_error_file_name :: Ptr H5EError -> Ptr CString
h5e_error_file_name p = castPtr $ plusPtr p (fromIntegral off_file_name)

h5e_error_desc :: Ptr H5EError -> Ptr CString
h5e_error_desc p = castPtr $ plusPtr p (fromIntegral off_desc)
 
----------------------------------------------------------------
-- Function pointers
----------------------------------------------------------------

-- | Callback to call in case of error
type H5EAuto = HID -> Ptr () -> IO HErr

-- | Callback for traversing error stack
type H5EWalk = CUInt -> Ptr H5EError -> Ptr () -> IO HErr


----------------------------------------------------------------
-- Wrapper functions
----------------------------------------------------------------

-- | Closes an error message.
foreign import capi "hdf5.h H5Eclose_msg" h5e_close_msg
  :: HID     -- ^ @err_id@ An error message identifier
  -> IO HErr -- ^ Returns a non-negative value if successful;
             --   otherwise returns a negative value.

-- | Closes an error stack handle. 
foreign import capi "hdf5.h H5Eclose_stack" h5e_close_stack
  :: HID     -- ^ @stack_id@ Error stack identifier
  -> IO HErr -- ^ Returns a non-negative value if successful;
             --   otherwise returns a negative value.




-- | Creates a new, empty error stack. Use H5Eclose_stack() to close
--   the error stack identifier returned by this function.
foreign import capi "hdf5.h H5Ecreate_stack" h5e_create_stack
  :: IO HID -- ^ Returns an error stack identifier if successful;
            --   otherwise returns @H5I_INVALID_HID@.

-- | Returns a copy of the current error stack.
foreign import capi "hdf5.h H5Eget_current_stack" h5e_get_current_stack
  :: IO HID -- ^ Returns an error stack identifier if successful;
            --   otherwise returns @H5I_INVALID_HID@.

-- | @H5Eget_auto2@ returns the settings for the automatic error stack
--   traversal function, @func@, and its data, @client_data@, that are
--   associated with the error stack specified by @estack_id@.
--
--   Either or both of the func and client_data arguments may be @NULL@,
--   in which case the value is not returned.
--
--   The library initializes its default error stack traversal
--   functions to @H5Eprint1@ and @H5Eprint2@. A call to
--   @H5Eget_auto2@ returns @H5Eprint2@ or the user-defined function
--   passed in through @H5Eset_auto2@.
foreign import ccall "hdf5.h H5Eget_auto2" h5e_get_auto
  :: HID          -- ^ @estack_id@ Error stack identifier
  -> Ptr (FunPtr H5EAuto)
     -- ^ @[out]@ @func@ The function currently set to be called upon
     --   an error condition
  -> Ptr (Ptr ()) -- ^ @client_data@ Data currently set to be passed
                  --   to the error function
  -> IO HErr
-- NOTE: capi chokes on function pointers

-- | Turns automatic error printing on or off.
--
--   @H5Eset_auto2@ turns on or off automatic printing of errors for
--   the error stack specified with estack_id. An estack_id value of
--   H5E_DEFAULT indicates the current stack.
--
--   When automatic printing is turned on, by the use of a non-null
--   func pointer, any API function which returns an error indication
--   will first call func, passing it client_data as an argument.
--
--   func, a function compliant with the @H5E_auto2_t@ prototype, is
--   defined in the @H5Epublic.h@ source code file as:
--
-- > typedef herr_t (*H5E_auto2_t)(hid_t estack, void *client_data);
--
--   When the library is first initialized, the auto printing function
--   is set to @H5Eprint2()@ (cast appropriately) and client_data is the
--   standard error stream pointer, @stderr@.
--
--   Automatic stack traversal is always in the @H5E_WALK_DOWNWARD@
--   direction.
--
--   Automatic error printing is turned off with a H5Eset_auto2() call
--   with a NULL func pointer.
foreign import capi "hdf5.h H5Eset_auto2" h5e_set_auto
  :: HID            -- ^ @estack_id@ Error stack identifier
  -> FunPtr H5EAuto -- ^ Function to be called upon an error condition
  -> Ptr ()         -- ^ @client_data@ Data passed to the error function
  -> IO HErr        -- ^ Returns a non-negative value if successful;
                    --   otherwise returns a negative value.



-- | @H5Ewalk2@ walks the error stack specified by err_stack for the
--   current thread and calls the function specified in func for each
--   error along the way.
--
--   If the value of err_stack is @H5E_DEFAULT@, then @H5Ewalk2@ walks
--   the current error stack.
--
--   direction specifies whether the stack is walked from the inside
--   out or the outside in. A value of @H5E_WALK_UPWARD@ means to begin
--   with the most specific error and end at the API; a value of
--   @H5E_WALK_DOWNWARD@ means to start at the API and end at the
--   innermost function where the error was first detected.
--
--   @func@, a function conforming to the @H5E_walk2_t@ prototype,
--   will be called for each error in the error stack. Its arguments
--   will include an index number n (beginning at zero regardless of
--   stack traversal direction), an error stack entry err_desc, and
--   the client_data pointer passed to @H5Eprint@. The @H5E_walk2_t@
--   prototype is as follows:
-- 
-- > typedef herr_t (*H5E_walk2_t)(unsigned n, const H5E_error2_t *err_desc, void *client_data);
foreign import capi "hdf5.h H5Ewalk2" h5e_walk
  :: HID            -- ^ @err_stack@ Error stack identifier 
  -> H5EDirection   -- ^ @direction@ Direction in which the error
                    --   stack is to be walked
  -> FunPtr H5EWalk -- ^ @func@ Function to be called for each error encountered 
  -> Ptr ()         -- ^ @client_data@ Data to be passed to func
  -> IO HErr        -- ^ Returns a non-negative value if successful;
                    --   otherwise returns a negative value.

-- | @H5Eget_msg@ retrieves the error message including its length and
--   type. The error message is specified by msg_id. The user is
--   responsible for passing in sufficient buffer space for the
--   message. If msg is not NULL and size is greater than zero, the
--   error message of size long is returned. The length of the message
--   is also returned. If NULL is passed in as msg, only the length
--   and type of the message is returned. If the return value is zero,
--   it means there is no message.
foreign import capi "hdf5.h H5Eget_msg" h5e_get_msg
  :: HID         -- ^ @msg_id@ Error message identifier 
  -> Ptr H5EType -- ^ @[out]@ @type@ The type of the error message
  -> Ptr CChar   -- ^ @msg@ Error message buffer
  -> CSize       -- ^ @size@ The length of error message to be returned by this function 
  -> IO CSize    -- ^ Returns the size of the error message in bytes
                 --   on success; otherwise returns a negative value.

{-
hid_t 	H5Eregister_class (const char *cls_name, const char *lib_name, const char *version)
herr_t 	H5Eunregister_class (hid_t class_id)
hid_t 	H5Ecreate_msg (hid_t cls, H5E_type_t msg_type, const char *msg)
herr_t 	H5Eappend_stack (hid_t dst_stack_id, hid_t src_stack_id, hbool_t close_source_stack)
ssize_t H5Eget_class_name (hid_t class_id, char *name, size_t size)
herr_t 	H5Eset_current_stack (hid_t err_stack_id)
herr_t 	H5Epush2 (hid_t err_stack, const char *file, const char *func, unsigned line, hid_t cls_id, hid_t maj_id, hid_t min_id, const char *msg,...)
herr_t 	H5Epop (hid_t err_stack, size_t count)
herr_t 	H5Eprint2 (hid_t err_stack, FILE *stream)
herr_t 	H5Eclear2 (hid_t err_stack)
herr_t 	H5Eauto_is_v2 (hid_t err_stack, unsigned *is_stack)
ssize_t H5Eget_num (hid_t error_stack_id)
herr_t 	H5Eclear1 (void)
herr_t 	H5Eget_auto1 (H5E_auto1_t *func, void **client_data)
herr_t 	H5Epush1 (const char *file, const char *func, unsigned line, H5E_major_t maj, H5E_minor_t min, const char *str)
herr_t 	H5Eprint1 (FILE *stream)
herr_t 	H5Eset_auto1 (H5E_auto1_t func, void *client_data)
-}
