module Hasura.Class
  ( MetadataStorageT(..)
  , runMetadataStorageT
  , MonadMetadataStorage(..)
  )
where

import           Hasura.Eventing.HTTP
import           Hasura.Eventing.ScheduledTrigger.Types
import           Hasura.Prelude
import           Hasura.RQL.Types

import           Control.Monad.Morph                    (MFunctor)

import qualified Hasura.Tracing                         as Tracing

newtype MetadataStorageT m a
  = MetadataStorageT {unMetadataStorageT :: ExceptT QErr m a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MFunctor
           , MonadTrans
           , MonadIO
           , Tracing.HasReporter
           )

runMetadataStorageT
  :: MetadataStorageT m a -> m (Either QErr a)
runMetadataStorageT =
  runExceptT . unMetadataStorageT


class (MonadError QErr m) => MonadMetadataStorage m where

  -- Scheduled triggers
  getDeprivedCronTriggerStats :: m [CronTriggerStats]
  getScheduledEventsForDelivery :: m ([CronEvent], [OneOffScheduledEvent])
  insertScheduledEvent :: ScheduledEventSeed -> m ()
  insertScheduledEventInvocation
    :: Invocation 'ScheduledType -> ScheduledEventType -> m ()
  setScheduledEventOp
    :: ScheduledEventId -> ScheduledEventOp -> ScheduledEventType -> m ()
  unlockScheduledEvents
    :: ScheduledEventType -> [ScheduledEventId] -> m Int
  unlockAllLockedScheduledEvents :: m ()

instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m) where

  getDeprivedCronTriggerStats        = lift getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = lift getScheduledEventsForDelivery
  insertScheduledEvent               = lift . insertScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = lift unlockAllLockedScheduledEvents

-- instance (MonadMetadataStorage m) => MonadMetadataStorage (ExceptT e m) where

--   getDeprivedCronTriggerStats        = lift getDeprivedCronTriggerStats
--   getScheduledEventsForDelivery      = lift getScheduledEventsForDelivery
--   insertScheduledEvent               = lift . insertScheduledEvent
--   insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
--   setScheduledEventOp a b c          = lift $ setScheduledEventOp a b c
--   unlockScheduledEvents a b          = lift $ unlockScheduledEvents a b
--   unlockAllLockedScheduledEvents     = lift unlockAllLockedScheduledEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (Tracing.TraceT m) where

  getDeprivedCronTriggerStats        = lift getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = lift getScheduledEventsForDelivery
  insertScheduledEvent               = lift . insertScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = lift unlockAllLockedScheduledEvents

-- instance (MonadMetadataStorage m) => MonadMetadataStorage (LazyTxT e m) where

--   getDeprivedCronTriggerStats        = lift getDeprivedCronTriggerStats
--   getScheduledEventsForDelivery      = lift getScheduledEventsForDelivery
--   insertScheduledEvent               = lift . insertScheduledEvent
--   insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
--   setScheduledEventOp a b c          = lift $ setScheduledEventOp a b c
--   unlockScheduledEvents a b          = lift $ unlockScheduledEvents a b
--   unlockAllLockedScheduledEvents     = lift unlockAllLockedScheduledEvents
