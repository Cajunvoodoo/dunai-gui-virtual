module FRP.Dynamic where


import FRP.BearRiver

-- | Dynamics allow for the subscription to @Events@ while tracking a current
-- value.
data Dynamic a = Dynamic
    { modified :: Event a
    , current :: a
    } deriving (Show, Functor)

-- | Retrieve the current value from a @Dynamic@. Functionally identical to
-- @modified@, but with a name that may make more sense.
updated :: Dynamic a -> Event a
updated = modified

-- | Holds an initial value, updating the value whenever an event is received.
holdDyn :: forall m a. Monad m => a -> SF m (Event a) (Dynamic a)
holdDyn a = proc ev -> do
  loopPre a loopHold -< ev
  where
    loopHold :: SF m (Event a, a) (Dynamic a, a)
    loopHold = proc (newEv, old) -> do
      case newEv of
        Event new -> returnA -< (Dynamic newEv new, new)
        NoEvent   -> returnA -< (Dynamic NoEvent old, old)

-- | Like @holdDyn@, but the value in the @Dynamic@ never changes.
constDyn :: Monad m => a -> SF m () (Dynamic a)
constDyn a = pure $ Dynamic NoEvent a

-- Cajun: Should dynamics even have an applicative instance?
-- instance Applicative Dynamic where
--     -- NB: 'pure' never fires an update event!
--     -- In most cases, 'constDyn' is more appropriate
--     pure = Dynamic False

--     df <*> dx = Dynamic
--         { modified = modified df || modified dx
--         , current = (current df) (current dx)
--         }
