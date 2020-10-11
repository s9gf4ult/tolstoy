module UsersExample.Data.V1 where

import           Control.Lens
import           Control.Monad
import           Data.Char as C
import           Data.Generics.Product
import           Data.Proxy
import           Data.Text as T
import           GHC.Generics (Generic)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Arbitrary.Generic
import           Tolstoy.Migration
import           Tolstoy.Migration.Obvious
import           Tolstoy.Structure
import           Tolstoy.Types
import           TypeFun.Data.Peano
import           UsersExample.Data.Shared
import qualified UsersExample.Data.V0 as V0

data User = User
  { name   :: Maybe Name
  , email  :: Email
  , status :: UserStatus
  , phone  :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance Structural User

instance Arbitrary User where
  arbitrary = genericArbitrary
  shrink = genericShrink

data UserStatus
  = Registered
  | Confirmed
  | Banned
  | Blessed
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary UserStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Structural UserStatus

initUser :: User
initUser = User
  { name   = Nothing
  , email  = "user@email"
  , status = Registered
  , phone = Nothing
  }

data UserAction
  = Init
  | SetName Name
  | SetEmail Email
  | Confirm
  | Ban
  | Bless
  | SetPhone Text
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary UserAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Structural UserAction


actionMigrations :: Migrations (S Z) '[ UserAction, V0.UserAction ]
actionMigrations
  = Migrate Proxy obviousMigration
  $ FirstVersion Proxy Proxy

userMigrations :: Migrations (S Z) '[ User, V0.User]
userMigrations
  = Migrate Proxy obviousMigration
  $ FirstVersion Proxy Proxy

userAction :: PureDocAction User UserAction
userAction = pureDocAction $ \user -> \case
  Init         -> return user
  SetName name -> do
    checkStatus user
    return $ user & field @"name" .~ Just name
  SetEmail e -> do
    checkStatus user
    return $ user & field @"email" .~ e
  Confirm -> do
    checkStatus user
    return $ user & field @"status" .~ Confirmed
  Ban -> return $ user & field @"status" .~ Banned
  Bless -> do
    checkStatus user
    return $ user & field @"status" .~ Blessed
  SetPhone p -> do
    unless (T.all C.isDigit p) $ do
      Left "Phone is invalid"
    return $ user & field @"phone" .~ Just p
  where
    checkStatus user = case status user of
      Banned -> Left "User is banned"
      _      -> pure ()
