module Example where

import           Control.Lens
import           Data.Aeson
import           Data.Generics.Product
import           Data.Text                 as T
import           Database.PostgreSQL.Query
import           GHC.Generics              (Generic)
import           Tolstoy.DB

newtype UserId = UserId
  { unUserId :: Integer
  } deriving (Eq, Ord, Show, Generic, FromField, ToField)

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

userAction :: Action User UserAction
userAction user = \case
  SetName name -> do
    checkStatus
    return $ user & field @"name" .~ Just name
  Confirm -> do
    checkStatus
    return $ user & field @"status" .~ Confirmed
  Ban -> return $ user & field @"status" .~ Banned
  where
    checkStatus = case status user of
      Banned -> Left "User is banned"
      _      -> pure ()
