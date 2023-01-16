{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
module HDF5.C.H5G
  ( h5g_close
  , h5g_open
  , h5g_create
  ) where


import Foreign.C
-- import Foreign.Ptr
import HDF5.C.Types

-- | @H5Gclose@ releases resources used by a group which was opened by
--   H5Gcreate@ or @H5Gopen@. After closing a group, @group_id@ cannot
--   be used again until another @H5Gcreate@ or @H5Gopen@ is called on
--   it.
--
--   Failure to release a group with this call will result in resource
--   leaks.
foreign import capi "hdf5-hs.h hs_H5Gclose" h5g_close
  :: HID     -- ^ @group_id@ Group identifier
  -> HIO HErr


-- | @H5Gopen2@ opens an existing group, name, at the location
--   specified by @loc_id@.
--
--   @H5Gopen2@ returns a group identifier for the group that was
--   opened. This group identifier should be released by @H5Gclose@
--   when it is no longer needed to prevent resource leaks.
foreign import capi "hdf5-hs.h hs_H5Gopen2" h5g_open
  :: HID     -- ^ @loc_id@ Location identifier. The identifier may be that
             --   of a file, group, dataset, named datatype, or attribute.
  -> CString -- ^ @name@ Name of the group to open 
  -> HID     -- ^ @gapl_id@ Group access property list identifier
  -> HIO HID -- ^ Returns a group identifier if successful; otherwise
             --   returns H5I_INVALID_HID.


-- | @H5Gcreate2@ creates a new group in a file. After a group has
--   been created, links to datasets and to other groups can be added.
--
--   The @loc_id@ and @name@ parameters specify where the group is
--   located. loc_id may be a file, group, dataset, named datatype or
--   attribute in the file. If an attribute, dataset, or named
--   datatype is specified for loc_id then the group will be created
--   at the location where the attribute, dataset, or named datatype
--   is attached. @name@ is the link to the group; name may be either an
--   absolute path in the file (the links from the root group to the
--   new group) or a relative path from @loc_id@ (the link(s) from the
--   group specified by @loc_id@ to the new group).
--
--   @lcpl_id@, @gcpl_id@, and @gapl_id@ are property list
--   identifiers. These property lists govern how the link to the
--   group is created, how the group is created, and how the group can
--   be accessed in the future, respectively. @H5P_DEFAULT@ can be
--   passed in if the default properties are appropriate for these
--   property lists. Currently, there are no APIs for the group access
--   property list; use @H5P_DEFAULT@.
--
--   The group identifier should be closed by @H5Gclose@ when access
--   is no longer required to prevent resource leaks.
foreign import capi "hdf5-hs.h hs_H5Gcreate2" h5g_create
  :: HID     -- ^ @loc_id@ Location identifier. The identifier may be
             --   that of a file, group, dataset, named datatype, or
             --   attribute.
  -> CString -- ^ @name@ Name of the group to create
  -> HID     -- ^ @lcpl_id@ Link creation property list identifier
  -> HID     -- ^ @gcpl_id@ Group creation property list identifier
  -> HID     -- ^ @gapl_id@ Group access property list identifier
  -> HIO HID -- ^ Returns a group identifier if successful; otherwise
             --   returns @H5I_INVALID_HID@.

{-
hid_t 	H5Gcreate2 (hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id)
hid_t 	H5Gcreate_anon (hid_t loc_id, hid_t gcpl_id, hid_t gapl_id)
hid_t 	H5Gopen2 (hid_t loc_id, const char *name, hid_t gapl_id)
hid_t 	H5Gget_create_plist (hid_t group_id)
herr_t 	H5Gget_info (hid_t loc_id, H5G_info_t *ginfo)
herr_t 	H5Gget_info_by_name (hid_t loc_id, const char *name, H5G_info_t *ginfo, hid_t lapl_id)
herr_t 	H5Gget_info_by_idx (hid_t loc_id, const char *group_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n, H5G_info_t *ginfo, hid_t lapl_id)
herr_t 	H5Gflush (hid_t group_id)
herr_t 	H5Grefresh (hid_t group_id)
-}
