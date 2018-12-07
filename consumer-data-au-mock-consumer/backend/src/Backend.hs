{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Snap.Core (MonadSnap)

backendServe :: MonadSnap m => BackendRoute () -> m ()
backendServe = \case
  BackendRoute_Missing -> return ()
  BackendRoute_Balance -> return ()

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \a -> return ()
  , _backend_routeEncoder = backendRouteEncoder
  }
