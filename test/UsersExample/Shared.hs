module UsersExample.Shared where

import Control.Monad.Fail
import Control.Monad.Logger
import Data.Pool as Pool
import Database.PostgreSQL.Query


type TestMonad = PgMonadT (LoggingT IO)

instance MonadFail TestMonad where
  fail = error

runTest :: Pool Connection -> TestMonad a -> IO a
runTest p t = do
  runStderrLoggingT $ Pool.withResource p $ \con -> runPgMonadT con t
