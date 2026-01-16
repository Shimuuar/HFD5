{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module HDF5.C.H5L
  ( -- * Data types
    -- ** H5LInfo
    H5LInfo2
    -- ** H5LType
  , H5LType(..)
  , pattern H5L_TYPE_ERROR
  , pattern H5L_TYPE_HARD
  , pattern H5L_TYPE_SOFT
  , pattern H5L_TYPE_EXTERNAL
  , pattern H5L_TYPE_MAX
    -- ** Callbacks
  , H5LIterate2
  , makeH5LIterate2
    -- * Functions
  , h5l_iterate
  , h5l_delete
  ) where


import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import HDF5.C.Types

----------------------------------------------------------------
-- H5LInfo
----------------------------------------------------------------

-- | Tag for H5L_info2_t
--
--   NOTE: It was introduced in 1.12. We use compatibility macro in
--   order to support 1.10
data H5LInfo2


----------------------------------------------------------------
-- H5LType
----------------------------------------------------------------

-- | Link class types.
-- 
--   Values less than 64 are reserved for the HDF5 library's internal
--   use. Values 64 to 255 are for "user-defined" link class types;
--   these types are defined by HDF5 but their behavior can be
--   overridden by users. Users who want to create new classes of
--   links should contact the HDF5 development team at
--   help@hdfgroup.org. These values can never change because they
--   appear in HDF5 files.
newtype H5LType = H5LType CInt
  deriving stock   (Show)
  deriving newtype (Eq,Ord,Storable)

foreign import capi "hdf5.h value H5L_TYPE_ERROR"    h5l_TYPE_ERROR    :: H5LType
foreign import capi "hdf5.h value H5L_TYPE_HARD"     h5l_TYPE_HARD     :: H5LType
foreign import capi "hdf5.h value H5L_TYPE_SOFT"     h5l_TYPE_SOFT     :: H5LType
foreign import capi "hdf5.h value H5L_TYPE_EXTERNAL" h5l_TYPE_EXTERNAL :: H5LType
foreign import capi "hdf5.h value H5L_TYPE_MAX"      h5l_TYPE_MAX      :: H5LType

-- | Invalid link type id
pattern H5L_TYPE_ERROR :: H5LType
pattern H5L_TYPE_ERROR <- ((==h5l_TYPE_ERROR) -> True) where H5L_TYPE_ERROR = h5l_TYPE_ERROR

-- | Hard link id
pattern H5L_TYPE_HARD :: H5LType
pattern H5L_TYPE_HARD <- ((==h5l_TYPE_HARD) -> True) where H5L_TYPE_HARD = h5l_TYPE_HARD

-- | Soft link id
pattern H5L_TYPE_SOFT :: H5LType
pattern H5L_TYPE_SOFT <- ((==h5l_TYPE_SOFT) -> True) where H5L_TYPE_SOFT = h5l_TYPE_SOFT

-- | External link id
pattern H5L_TYPE_EXTERNAL :: H5LType
pattern H5L_TYPE_EXTERNAL <- ((==h5l_TYPE_EXTERNAL) -> True) where H5L_TYPE_EXTERNAL = h5l_TYPE_EXTERNAL

-- | Maximum link type id 
pattern H5L_TYPE_MAX :: H5LType
pattern H5L_TYPE_MAX <- ((==h5l_TYPE_MAX) -> True) where H5L_TYPE_MAX = h5l_TYPE_MAX


----------------------------------------------------------------
-- Callbacks
----------------------------------------------------------------

type H5LIterate2 x
  =  HID          -- Group
  -> CString      -- Name
  -> Ptr H5LInfo2 -- information on node
  -> Ptr x
  -> IO HErr

foreign import ccall "wrapper"
  makeH5LIterate2 :: H5LIterate2 x -> IO (FunPtr (H5LIterate2 x))


----------------------------------------------------------------
-- Functions
----------------------------------------------------------------


-- | @H5Literate2@ iterates through the links in a file or group,
--   @group_id@, in the order of the specified index, idx_type, using a
--   user-defined callback routine op. @H5Literate2@ does not
--   recursively follow links into subgroups of the specified group.
--
--   Three parameters are used to manage progress of the iteration:
--   @idx_type@, @order@, and @idx_p@.
--
--   @idx_type@ specifies the index to be used. If the links have not
--   been indexed by the index type, they will first be sorted by that
--   index then the iteration will begin; if the links have been so
--   indexed, the sorting step will be unnecessary, so the iteration
--   may begin more quickly.
--
--   @order@ specifies the order in which objects are to be inspected
--   along the index idx_type.
--
--   @idx_p@ tracks the iteration and allows an iteration to be
--   resumed if it was stopped before all members were processed. It
--   is passed in by the application with a starting point and
--   returned by the library with the point at which the iteration
--   stopped.
--
--   @op_data@ is a user-defined pointer to the data required to
--   process links in the course of the iteration. This pointer is
--   passed back to each step of the iteration in the op callback
--   function's op_data parameter. op is invoked for each link
--   encounter.
--
--   @op_data@ is passed to and from each iteration and can be used to
--   supply or aggregate information across iterations.
--
-- **Warning** The behavior of @H5Literate1@ is undefined if the link
-- membership of group_id changes during the iteration. This does not
-- limit the ability to change link destinations while iterating, but
-- caution is advised.
foreign import capi "hdf5-hs.h hs_H5Literate" h5l_iterate
  :: HID         -- ^ @grp_id@ Group identifier
  -> H5Index     -- ^ @idx_type@ Index type
  -> H5IterOrder -- ^ @order@ Iteration order
  -> Ptr HSize   -- ^ @[in,out]@ @idx@ Pointer to an iteration index to allow continuing a previous iteration
  -> FunPtr (H5LIterate2 x)
     -- ^ @op@ Callback function
  -> Ptr x       -- ^ @op_data@ User-defined callback function context
  -> HIO HErr
     -- ^ *Success:* The return value of the first operator that
     --   returns non-zero, or zero if all members were processed with
     --   no operator returning non-zero.
     --
     --   *Failure*: Negative if an error occurs in the library, or
     --   the negative value returned by one of the operators.


-- | Returns a non-negative value if successful; otherwise, returns a negative value.
--
--   H5Ldelete() removes the link specified by name from the location
--   loc_id.
--
--   If the link being removed is a hard link, H5Ldelete() also
--   decrements the link count for the object to which name
--   points. Unless there is a duplicate hard link in that group, this
--   action removes the object to which name points from the group
--   that previously contained it.
--
--   Object headers keep track of how many hard links refer to an
--   object; when the hard link count, also referred to as the
--   reference count, reaches zero, the object can be removed from the
--   file. The file space associated will then be released, i.e.,
--   identified in memory as freespace. Objects which are open are not
--   removed until all identifiers to the object are closed.
foreign import capi "hdf5-hs.h hs_H5Ldelete" h5l_delete
  :: HID     -- ^ [in] loc_id Location identifier. The identifier may
             -- be that of a file, group, dataset, named datatype, or
             -- attribute.
  -> CString -- ^ [in] name Name of the link to delete 
  -> HID     -- ^ [in] lapl_id	Link access property list identifier
  -> HIO HErr
