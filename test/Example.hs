module Example where

import Control.Lens
import Data.Aeson
import Data.Generics.Product
import Data.Text as T
import Database.PostgreSQL.Query
import GHC.Generics (Generic)
import Tolstoy.DB

data User = User
  { name   :: Maybe Text
  , email  :: Text
  , status :: UserStatus
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON User
instance FromJSON User

data UserStatus
  = Registered
  | Confirmed
  | Banned
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserStatus
instance FromJSON UserStatus

data UserAction
  = SetName Text
  | Confirm
  | Ban
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserAction
instance FromJSON UserAction

userAction :: PureDocAction User UserAction
userAction = pureDocAction $ \user -> \case
  SetName name -> do
    checkStatus user
    return $ user & field @"name" .~ Just name
  Confirm -> do
    checkStatus user
    return $ user & field @"status" .~ Confirmed
  Ban -> return $ user & field @"status" .~ Banned
  where
    checkStatus user = case status user of
      Banned -> Left "User is banned"
      _      -> pure ()
