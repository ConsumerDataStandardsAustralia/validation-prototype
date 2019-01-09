{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

module Web.ConsumerData.Au.LambdaBank.AuthModel where

import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVarIO, writeTVar)
import Control.Monad.Free          (MonadFree, liftF)
import Control.Monad.Free.Church   (F)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Reader.Class  (MonadReader, ask)

import Data.Functor.Coproduct ((:<:), inj)

data AuthModelF next where
  IncrementCount :: (Integer -> next) -> AuthModelF next

deriving instance Functor AuthModelF

incrementCount ::
  ( AuthModelF :<: f
  , MonadFree f m
  , Functor f
  )
  => m Integer
incrementCount =
  liftF . inj $ IncrementCount id

type AuthModelM = F AuthModelF

runAuthModelF ::
  ( MonadIO m
  , MonadReader (TVar Integer) m
  )
  => AuthModelF a
  -> m a
runAuthModelF = \case
  (IncrementCount next) -> do
    counterVar <- ask
    counter <- liftIO . fmap succ $ readTVarIO counterVar
    liftIO . atomically $ writeTVar counterVar counter
    pure $ next counter

authProg :: AuthModelM Integer
authProg = do
  _ <- incrementCount
  incrementCount
