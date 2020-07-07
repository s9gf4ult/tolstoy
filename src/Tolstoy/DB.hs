module Tolstoy.DB where

import           Data.Aeson
import           Data.Text                 as T
import           Database.PostgreSQL.Query

type Action obj act = obj -> act -> Either Text obj

data Tolstoy obj objId act = Tolstoy
  { insertObject :: obj -> IO objId
  , getObject    :: objId -> IO obj
  , changeObject :: objId -> act -> IO obj
  }

tolstoy
  :: forall act obj objId
  .  (Action obj act)
  -> Tolstoy act obj objId
tolstoy = error "FIXME: tolstoy not implemented"
