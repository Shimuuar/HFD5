{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}
-- |
module HDF5.HL.Internal.Error
  ( -- * Exception data type
    Error(..)
  , Message(..)
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
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Foreign.C
import Text.Printf
import GHC.Stack

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
    , [ ' ':' ':prettyCallSite s | s <- reverse $ getCallStack callStack]
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

checkCLLong :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO CLLong) -> IO CLLong
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
