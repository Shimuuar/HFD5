{-# LANGUAGE RecordWildCards #-}
-- |
module HDF5.HL.Unsafe.Error
  ( -- * Exception data type
    Error(..)
  , Message(..)
  , DataspaceParseError(..)
    -- * API
  , decodeError
  , checkHID
  , checkHErr
  , checkHTri
  , checkCInt
  , checkCLLong
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.IORef
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Foreign.C
import Text.Printf
import GHC.Stack
import GHC.Generics (Generic)

import HDF5.C

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Error during HDF5 call
data Error where
  Error :: HasCallStack => [Either String Message] -> Error

-- GHC display exception using show instead of displayException. No
-- way around this. We have to override Show
--
-- See https://mail.haskell.org/pipermail/libraries/2018-May/028813.html
-- for a bit of history
instance Show Error where
  show (Error msgs) = unlines $ concat
    [ [ "HDF5 error" ]
    , [ ' ':' ':prettyCallSite s | s <- getCallStack callStack]
    , either displayMsgHS displayMsg =<< msgs
    ]
    where
      prettyCallSite (f, loc) = f ++ ", called at " ++ prettySrcLoc loc
      displayMsg Message{..} =
        [ printf "%s (%s:%i): %s" msgFunc msgFile msgLine msgDescr
        , printf "  Major: %s" msgMajor
        , printf "  Minor: %s" msgMinor
        ]
      displayMsgHS msg = [msg]

-- | Major error codes for HDF5 error. Here we follow naming
--   conventions used by HDF5
data MajError
  = MAJ_ARGS       -- ^ Invalid arguments to routine
  | MAJ_RESOURCE   -- ^ Resource unavailable
  | MAJ_INTERNAL   -- ^ Internal error (too specific to document in detail)
  | MAJ_LIB        -- ^ General library infrastructure
  | MAJ_FILE       -- ^ File accessibility
  | MAJ_IO         -- ^ Low-level I/O
  | MAJ_FUNC       -- ^ Function entry/exit
  | MAJ_ID         -- ^ Object ID
  | MAJ_CACHE      -- ^ Object cache
  | MAJ_LINK       -- ^ Links
  | MAJ_BTREE      -- ^ B-Tree node
  | MAJ_SYM        -- ^ Symbol table
  | MAJ_HEAP       -- ^ Heap
  | MAJ_OHDR       -- ^ Object header
  | MAJ_DATATYPE   -- ^ Datatype
  | MAJ_DATASPACE  -- ^ Dataspace
  | MAJ_DATASET    -- ^ Dataset
  | MAJ_STORAGE    -- ^ Data storage
  | MAJ_PLIST      -- ^ Property lists
  | MAJ_ATTR       -- ^ Attribute
  | MAJ_PLINE      -- ^ Data filters
  | MAJ_EFL        -- ^ External file list
  | MAJ_REFERENCE  -- ^ References
  | MAJ_VFL        -- ^ Virtual File Layer
  | MAJ_VOL        -- ^ Virtual Object Layer
  | MAJ_TST        -- ^ Ternary Search Trees
  | MAJ_RS         -- ^ Reference Counted Strings
  | MAJ_ERROR      -- ^ Error API
  | MAJ_SLIST      -- ^ Skip Lists
  | MAJ_FSPACE     -- ^ Free Space Manager
  | MAJ_SOHM       -- ^ Shared Object Header Messages
  | MAJ_EARRAY     -- ^ Extensible Array
  | MAJ_FARRAY     -- ^ Fixed Array
  | MAJ_PLUGIN     -- ^ Plugin for dynamically loaded library
  | MAJ_PAGEBUF    -- ^ Page Buffering
  | MAJ_CONTEXT    -- ^ API Context
  | MAJ_MAP        -- ^ Map
  | MAJ_EVENTSET   -- ^ Event Set
  | MAJ_NONE_MAJOR -- ^ No error
  | MAJ_UNKNOWN    -- ^ We failed to map error code from HDF5 to
                   --   haskell type. If it appears in error this means
                   --   there's bug in bindings.
  deriving stock (Show,Generic)


decodeMajError :: HID -> MajError
decodeMajError h
  | h == c_H5E_ARGS       = MAJ_ARGS
  | h == c_H5E_RESOURCE   = MAJ_RESOURCE
  | h == c_H5E_INTERNAL   = MAJ_INTERNAL
  | h == c_H5E_LIB        = MAJ_LIB
  | h == c_H5E_FILE       = MAJ_FILE
  | h == c_H5E_IO         = MAJ_IO
  | h == c_H5E_FUNC       = MAJ_FUNC
  | h == c_H5E_ID         = MAJ_ID
  | h == c_H5E_CACHE      = MAJ_CACHE
  | h == c_H5E_LINK       = MAJ_LINK
  | h == c_H5E_BTREE      = MAJ_BTREE
  | h == c_H5E_SYM        = MAJ_SYM
  | h == c_H5E_HEAP       = MAJ_HEAP
  | h == c_H5E_OHDR       = MAJ_OHDR
  | h == c_H5E_DATATYPE   = MAJ_DATATYPE
  | h == c_H5E_DATASPACE  = MAJ_DATASPACE
  | h == c_H5E_DATASET    = MAJ_DATASET
  | h == c_H5E_STORAGE    = MAJ_STORAGE
  | h == c_H5E_PLIST      = MAJ_PLIST
  | h == c_H5E_ATTR       = MAJ_ATTR
  | h == c_H5E_PLINE      = MAJ_PLINE
  | h == c_H5E_EFL        = MAJ_EFL
  | h == c_H5E_REFERENCE  = MAJ_REFERENCE
  | h == c_H5E_VFL        = MAJ_VFL
  | h == c_H5E_VOL        = MAJ_VOL
  | h == c_H5E_TST        = MAJ_TST
  | h == c_H5E_RS         = MAJ_RS
  | h == c_H5E_ERROR      = MAJ_ERROR
  | h == c_H5E_SLIST      = MAJ_SLIST
  | h == c_H5E_FSPACE     = MAJ_FSPACE
  | h == c_H5E_SOHM       = MAJ_SOHM
  | h == c_H5E_EARRAY     = MAJ_EARRAY
  | h == c_H5E_FARRAY     = MAJ_FARRAY
  | h == c_H5E_PLUGIN     = MAJ_PLUGIN
  | h == c_H5E_PAGEBUF    = MAJ_PAGEBUF
  | h == c_H5E_CONTEXT    = MAJ_CONTEXT
  | h == c_H5E_MAP        = MAJ_MAP
  | h == c_H5E_EVENTSET   = MAJ_EVENTSET
  | h == c_H5E_NONE_MAJOR = MAJ_NONE_MAJOR
  | otherwise        = MAJ_UNKNOWN


data Message = Message
  { msgDescr :: String
  , msgMajor :: String
  , msgMinor :: String
  , msgLine  :: Int
  , msgFunc  :: String
  , msgFile  :: String
  }
  deriving stock Show

instance Exception Error

-- | Error during conversion of dataspace's size to haskell data type.
data DataspaceParseError
  = BadRank ![(Word64,Word64)]  -- ^ Has invalid shape
  | UnexpectedNull              -- ^ Cannot convert NULL dataspace to haskell type
  | BadIndex ![(Word64,Word64)] -- ^ Cannot convert index to haskell data type
  deriving stock Show

instance Exception DataspaceParseError


-- | Decode error from HDF5 error stack
decodeError :: HasCallStack => Ptr HID -> String -> IO Error
decodeError p_err msg = evalContT $ do
  hid_err  <- lift  $ peek p_err
  v_stack  <- lift  $ newIORef []
  buf      <- ContT $ allocaArray $ fromIntegral $ msg_size + 1
  let step _ p _ = do
        msgMajor <- do m_maj <- peek $ h5e_error_maj_num p
                       n     <- h5e_get_msg m_maj nullPtr buf msg_size p_err
                       if | n > 0     -> peekCString buf
                          | otherwise -> pure ""
        msgMinor <- do m_min <- peek $ h5e_error_min_num p
                       n     <- h5e_get_msg m_min nullPtr buf msg_size p_err
                       if | n > 0     -> peekCString buf
                          | otherwise -> pure ""
        msgFunc  <- peekCString  =<< peek (h5e_error_func_name p)
        msgFile  <- peekCString  =<< peek (h5e_error_file_name p)
        msgDescr <- peekCString  =<< peek (h5e_error_desc      p)
        msgLine  <- fromIntegral <$> peek (h5e_error_line      p)
        modifyIORef' v_stack (Right Message{..}:)
        pure $ HErr 0
  callback <- ContT $ bracket (makeWalker step) freeHaskellFunPtr
  res      <- lift  $ h5e_walk hid_err H5E_WALK_UPWARD callback nullPtr p_err
  case res of
    HOK      -> lift $ Error . (Left msg:) <$> readIORef v_stack
    HErrored -> pure $ Error [Left internal, Left msg]
  where
    -- Error message from major/minor labels are usually short so we
    -- don't need to bother with size discovery
    msg_size = 255
    internal = "INTERNAL: Failed to decode HDF5 error"

checkHID :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO HID) -> IO HID
{-# INLINE checkHID #-}
checkHID p_err msg action =
  action p_err >>= \case
    hid | hid < (HID 0) -> throwM =<< decodeError p_err msg
        | otherwise     -> pure hid

checkHErr :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO HErr) -> IO ()
{-# INLINE checkHErr #-}
checkHErr p_err msg action =
  action p_err >>= \case
    HOK -> pure ()
    _   -> throwM =<< decodeError p_err msg


checkCInt :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO CInt) -> IO CInt
{-# INLINE checkCInt #-}
checkCInt p_err msg action =
  action p_err >>= \case
    n | n < 0     -> throwM =<< decodeError p_err msg
      | otherwise -> pure n 

checkCLLong :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO HSSize) -> IO HSSize
{-# INLINE checkCLLong #-}
checkCLLong p_err msg action =
  action p_err >>= \case
    n | n < 0     -> throwM =<< decodeError p_err msg
      | otherwise -> pure n 

checkHTri :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO HTri) -> IO Bool
{-# INLINE checkHTri #-}
checkHTri p_err msg action =
  action p_err >>= \case
    HFalse -> pure False
    HTrue  -> pure True
    HFail  -> throwM =<< decodeError p_err msg
