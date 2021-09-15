{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Debug (
  DebugC,
  runDebug,
  ignoreDebug,
  recording,
  ignoring,
  Scope (..),
  module X,
) where

import Control.Carrier.Diagnostics
import Control.Carrier.Output.IO
import Control.Carrier.Simple (Simple, sendSimple)
import Control.Effect.Debug as X
import Control.Effect.Lift
import Control.Effect.Record (Recordable, SomeEffectResult (SomeEffectResult), recordEff)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Fixed
import Data.Text (Text)
import Data.Time.Clock.System (SystemTime (MkSystemTime), getSystemTime)
import Path

newtype DebugC m a = DebugC {runDebugC :: OutputC ScopeEvent m a}
  deriving (Functor, Applicative, Monad, MonadIO) -- TODO: MonadTrans

newtype Duration = Duration {unDuration :: Nano}
  deriving (Show)

data Scope = Scope
  { scopeTiming :: Duration
  , scopeEvents :: [ScopeEvent]
  , scopeFiles :: [ScopeFile]
  }
  deriving (Show)

instance ToJSON Scope where
  toJSON = object . scopePairs

scopePairs :: Scope -> [Pair]
scopePairs Scope{..} =
  [ "duration" .= show (unDuration scopeTiming)
  , "events" .= scopeEvents
  ]
    ++ whenNonEmpty "files" scopeFiles

whenNonEmpty :: ToJSON a => Text -> [a] -> [Pair]
whenNonEmpty _ [] = []
whenNonEmpty key val = [key .= val]

data ScopeEvent
  = EventEffect SomeEffectResult
  | EventScope Text Scope
  | EventError SomeDiagnostic

instance ToJSON ScopeEvent where
  toJSON (EventEffect (SomeEffectResult k v)) =
    object
      [ "effect" .= encodedK
      , "result" .= encodedV
      ]
    where
      (encodedK, encodedV) = recordEff k v
  toJSON (EventError (SomeDiagnostic _ err)) =
    object
      [ "error" .= show (renderDiagnostic err)
      ]
  toJSON (EventScope nm scope) =
    object $ ("scope" .= nm) : scopePairs scope

instance Show ScopeEvent where -- FIXME
  show (EventEffect (SomeEffectResult k v)) = "SomeEffectResult " <> show (recordEff k v)
  show (EventScope txt sc) = "EventScope " <> show txt <> " " <> show sc
  show (EventError (SomeDiagnostic _ err)) = "EventError " <> show (renderDiagnostic err)

data ScopeFile = ScopeFile (Path Abs Dir) Text
  deriving (Show)

instance ToJSON ScopeFile where
  toJSON _ = toJSON @Text "TODO"

timeBetween :: SystemTime -> SystemTime -> Duration
timeBetween (MkSystemTime sec ns) (MkSystemTime sec' ns') =
  Duration (realToFrac (sec' - sec) + MkFixed (fromIntegral ns' - fromIntegral ns))

runDebug :: Has (Lift IO) sig m => DebugC m a -> m (Scope, a)
runDebug act = do
  before <- sendIO getSystemTime
  (evs, res) <- runOutput @ScopeEvent $ runDebugC act
  after <- sendIO getSystemTime
  let duration = timeBetween before after
  pure (Scope duration evs [], res)

instance Has (Lift IO) sig m => Algebra (Debug :+: sig) (DebugC m) where
  alg hdl sig ctx = DebugC $
    case sig of
      L (DebugScope nm act) -> do
        let act' = hdl (act <$ ctx)
        (inner, res) <- lift $ runDebug act'
        output (EventScope nm inner)
        pure res
      L (DebugEffect k v) -> do
        output (EventEffect (SomeEffectResult k v))
        pure ctx
      L (DebugError err) -> do
        output (EventError (SomeDiagnostic [] err)) -- FIXME: empty path?
        pure ctx
      L (DebugBuildtool _) -> pure ctx -- FIXME
      L (DebugFile _) -> pure ctx -- FIXME
      R other -> alg (runDebugC . hdl) (R other) ctx

-----------------------------------------------

newtype IgnoreDebugC m a = IgnoreDebugC {runIgnoreDebugC :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Algebra sig m => Algebra (Debug :+: sig) (IgnoreDebugC m) where
  alg hdl sig ctx = IgnoreDebugC $
    case sig of
      L (DebugScope _ act) -> runIgnoreDebugC (hdl (act <$ ctx))
      L DebugError{} -> pure ctx
      L DebugEffect{} -> pure ctx
      L DebugBuildtool{} -> pure ctx
      L DebugFile{} -> pure ctx
      R other -> alg (runIgnoreDebugC . hdl) other ctx

ignoreDebug :: IgnoreDebugC m a -> m a
ignoreDebug = runIgnoreDebugC

-----------------------------------------------

-- | Wrap an effect invocation, recording its input/output values with 'debugEffect'
recording :: (Recordable r, Has Debug sig m, Has (Simple r) sig m) => r a -> m a
recording r = do
  res <- sendSimple r
  debugEffect r res
  pure res

-- | Ignore an effect invocation (just @send@ it)
ignoring :: Has (Simple r) sig m => r a -> m a
ignoring = sendSimple
{-# INLINE ignoring #-}
