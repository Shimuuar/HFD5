{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Foreign.Ptr
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
decodeError :: String -> HIO Error
decodeError msg = evalContT $ do
  v_stack  <- lift  $ hioNewIORef []
  buf      <- ContT $ hioAllocaArray $ fromIntegral $ msg_size + 1
  let step _ p _ = do
        msgMajor <- do m_maj <- hioPeek $ h5e_error_maj_num p
                       n     <- h5e_get_msg m_maj nullPtr buf msg_size
                       if | n > 0     -> hioPeekCString buf
                          | otherwise -> pure ""
        msgMinor <- do m_min <- hioPeek $ h5e_error_min_num p
                       n     <- h5e_get_msg m_min nullPtr buf msg_size
                       if | n > 0     -> hioPeekCString buf
                          | otherwise -> pure ""
        msgFunc  <- hioPeekCString =<< hioPeek (h5e_error_func_name p)
        msgFile  <- hioPeekCString =<< hioPeek (h5e_error_file_name p)
        msgDescr <- hioPeekCString =<< hioPeek (h5e_error_desc      p)
        msgLine  <- fromIntegral <$> hioPeek (h5e_error_line p)
        hioModifyIORef' v_stack (Message{..}:)
        pure $ HErr 0
  callback <- hioWithHaskellFunPtr $ makeWalker step
  res      <- lift  $ h5e_walk h5e_DEFAULT H5E_WALK_DOWNWARD callback nullPtr
  case res of
    HOK      -> lift $ Error msg <$> hioReadIORef v_stack
    HErrored -> pure $ InternalErr $ unlines ["Failed to decode HDF5 error for", msg]
  where
    -- Error message from major/minor labels are usually short so we
    -- don't need to bother with size discovery
    msg_size = 255

checkHID :: String -> HID -> HIO HID
checkHID msg hid
  | hid < (HID 0) = throwM =<< decodeError msg
  | otherwise       = pure hid

checkHErr :: String -> HErr -> HIO ()
checkHErr _   HOK      = pure ()
checkHErr msg HErrored = throwM =<< decodeError msg
