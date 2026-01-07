-- |
-- API for working with attributes of datasets\/groups.
module HDF5.HL.Attribute
  ( -- * Attributes
    Attribute
  , openAttrMay
  , withAttrMay
  , readAttrMay
  , writeAttr
    -- * Type class
  , SerializeAttr(..)
  , AttributeM
  , runAttributeM
  , encodeAttr
  , decodeAttr
  , attrSubset
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Foreign.C.String
import Foreign.Marshal
import GHC.Stack

import HDF5.HL.Unsafe.Types
import HDF5.HL.Unsafe.Wrappers
import HDF5.HL.Unsafe.Error
import HDF5.HL.Internal (ArrayLike(..), basicReadObject, basicWriteObject)
import HDF5.HL.Dataspace
import HDF5.C

----------------------------------------------------------------
-- Simple API
----------------------------------------------------------------

-- | Open attribute on group or dataset. Returns nothing if dataset
--   does not exists.
openAttrMay
  :: (HasAttrs d, MonadIO m, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> m (Maybe Attribute)
openAttrMay (getHID -> hid) path = liftIO $ withFrozenCallStack $ evalContT $ do
  p_err <- ContT $ alloca
  c_str <- ContT $ withCString path
  lift $ do
    exists <- checkHTri p_err ("Cannot check whether attribute " ++ path ++ " exists")
            $ h5a_exists hid c_str
    case exists of
      False -> pure Nothing
      True  -> Just . Attribute
            <$> ( checkHID p_err ("Cannot open attribute " ++ path)
                $ h5a_open hid c_str H5P_DEFAULT)

-- | Bracket variant of 'openAttrMay'.
withAttrMay
  :: (HasAttrs d, MonadIO m, MonadMask m, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> (Maybe Attribute -> m b)
  -> m b
withAttrMay a path = bracket (openAttrMay a path) (mapM_ (liftIO . basicClose))


-- | Read attribute from. Return @Nothing@ if attribute doesn't
--   exists, and throws exception if it couldn't be decoded.
readAttrMay
  :: (ArrayLike a, HasAttrs d, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> IO (Maybe a)
readAttrMay d name = withAttrMay d name $ \case
  Just x  -> Just <$> basicReadObject x
  Nothing -> pure Nothing

-- | Create attribute.
writeAttr
  :: forall a d. (ArrayLike a, HasAttrs d, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> a      -- ^ Value to write
  -> IO ()
writeAttr d name a = withFrozenCallStack $ evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString name
  space  <- ContT $ withCreateDataspaceFromExtent (getExtent a)
  tid    <- ContT $ withType $ typeH5 @(ElementOf a)
  attr   <- ContT $ bracket
    ( fmap Attribute
    $ checkHID p_err ("Cannot create attribute " ++ name)
    $ h5a_create (getHID d) c_path tid (getHID space)
          H5P_DEFAULT
          H5P_DEFAULT)
    basicClose
  lift $ basicWriteObject attr a


----------------------------------------------------------------
-- Typeclass based API
----------------------------------------------------------------


-- | Values which could be serialized as set of attributes. Since
--   attributes live in flat namespace and type class based approach
--   basically require hierarchical namespace we hack around it using
--   'AttributeM'.
class SerializeAttr a where
  -- | Parser that reads from set of attributes.
  fromAttrs :: HasCallStack => AttributeM a
  -- | Encode value as a set of attributes.
  toAttrs   :: HasCallStack => a -> AttributeM ()

instance SerializeAttr () where
  toAttrs   = pure
  fromAttrs = pure ()


-- | Monad which allows to create hierarchical namespace in
--   attributes. This is done by applying transformation to each
--   attribute name before reading\/writing it.
newtype AttributeM a = AttributeM
  { unAttributeM :: forall d. HasAttrs d => d -> (FilePath -> FilePath) -> IO a }
  deriving stock Functor

-- | Evaluate attribute parser
runAttributeM :: HasAttrs d => d -> AttributeM a -> IO a
runAttributeM d (AttributeM f) = f d id

instance Applicative AttributeM where
  pure a = AttributeM $ \_ _ -> pure a
  (<*>)  = ap

instance Monad AttributeM where
  m >>= fun = AttributeM $ \d f -> do
    a <- unAttributeM m d f
    unAttributeM (fun a) d f

-- | Prepend prefix to all attribute names mentioned in action.
attrSubset
  :: FilePath     -- ^ Prefix
  -> AttributeM a -- ^ Action
  -> AttributeM a
attrSubset dir m = AttributeM $ \d fun -> unAttributeM m d ((dir++) . ('/':) . fun)

encodeAttr :: ArrayLike a => FilePath -> a -> AttributeM ()
encodeAttr name a = AttributeM $ \d fun -> do
  writeAttr d (fun name) a

decodeAttr :: ArrayLike a => FilePath -> AttributeM a
decodeAttr name = AttributeM $ \d fun -> do
  readAttrMay d (fun name) >>= \case
    Nothing -> error "No attribute" -- FIXME: proper error handling
    Just a  -> pure a
