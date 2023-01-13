{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
-- |
module HDF5.HL.Internal.Error
  ( -- * Exception data type
    Error(..)
  , Message(..)
    -- * API
  , decodeError
  , checkHID
  , checkHErr
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.IORef
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Foreign.C
import HDF5.C

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Error during HDF5 call
data Error
  = Error { errorDescr :: String
          , errorStack :: [Message]
          }
  | InternalErr String
    -- ^ Internal error in library.
  deriving stock Show

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
decodeError :: Ptr HID -> String -> IO Error
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
        modifyIORef' v_stack (Message{..}:)
        pure $ HErr 0
  callback <- ContT $ bracket (makeWalker step) freeHaskellFunPtr
  res      <- lift  $ h5e_walk hid_err H5E_WALK_DOWNWARD callback nullPtr p_err
  case res of
    HOK      -> lift $ Error msg <$> readIORef v_stack
    HErrored -> pure $ InternalErr $ unlines ["Failed to decode HDF5 error for", msg]
  where
    -- Error message from major/minor labels are usually short so we
    -- don't need to bother with size discovery
    msg_size = 255

checkHID :: String -> (Ptr HID -> IO HID) -> IO HID
checkHID msg action = alloca $ \p_err -> do
  action p_err >>= \case
    hid | hid < (HID 0) -> throwM =<< decodeError p_err msg
        | otherwise     -> pure hid

checkHErr :: String -> (Ptr HID -> IO HErr) -> IO ()
checkHErr msg action = alloca $ \p_err -> do
  action p_err >>= \case
    HOK -> pure ()
    _   -> throwM =<< decodeError p_err msg
