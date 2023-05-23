{-# LANGUAGE TemplateHaskell #-}
module Main where

import FRP.Dynamic
import FRP.Dunai.QuickCheck
import FRP.Dunai.Stream
import FRP.Dunai.LTLFuture
import FRP.Dunai.Debug
import FRP.BearRiver
import Test.QuickCheck


holdDynUpdate :: Monad m => Int -> TPred (ClockInfo m) (Event Int)
holdDynUpdate i = Always $ Prop
  (
    proc ev -> do
      (newDyn, oldDyn) <- holdDynPair i -< ev
      case ev of
        Event new -> returnA -< current newDyn == new
        NoEvent   -> returnA -< current newDyn == current oldDyn
  )

-- | Same as @holdDyn@, but holds the current and last value
holdDynPair :: forall a m. Monad m => a -> SF m (Event a) (Dynamic a, Dynamic a)
holdDynPair a = loopPre (Dynamic NoEvent a) loopHold
  where
    loopHold :: Monad m => SF m (Event a, Dynamic a) ((Dynamic a, Dynamic a), Dynamic a)
    loopHold = proc (newEv, oldDyn) -> do
      newDyn <- holdDyn a -< newEv
      returnA -< ((newDyn, oldDyn), newDyn)


prop_HoldDyn :: Property
prop_HoldDyn =
  forAll uniDistStream $ evalT @Maybe (holdDynUpdate 1)

-- Orphan ):
instance Arbitrary a => Arbitrary (Event a) where
  arbitrary = oneof
    [ Event <$> arbitrary
    , pure NoEvent
    ]

return []
runTests = $quickCheckAll

main = runTests
