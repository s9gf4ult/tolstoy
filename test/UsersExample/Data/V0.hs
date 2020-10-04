module UsersExample.Data.V0 where

import Control.Lens
import Data.Generics.Product
import Data.Proxy
import Data.String
import Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen as Arb
import Tolstoy.DB
import Tolstoy.Migration
import Tolstoy.Structure
import Tolstoy.Types
import UsersExample.Data.Shared

data User = User
  { name   :: Maybe Name
  , email  :: Email
  , status :: UserStatus
  } deriving (Eq, Ord, Show, Generic)

instance Structural User

instance Arbitrary User where
  arbitrary = genericArbitrary
  shrink = genericShrink

data UserStatus
  = Registered
  | Confirmed
  | Banned
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary UserStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Structural UserStatus

initUser :: User
initUser = User
  { name   = Nothing
  , email  = "user@email"
  , status = Registered }

data UserAction
  = Init
  | SetName Name
  | SetEmail Email
  | Confirm
  | Ban
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary UserAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Structural UserAction

actionMigrations :: Migrations 0 '[ UserAction ]
actionMigrations = LastVersion Proxy Proxy

userMigrations :: Migrations 0 '[ User ]
userMigrations = LastVersion Proxy Proxy

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
  where
    checkStatus user = case status user of
      Banned -> Left "User is banned"
      _      -> pure ()
