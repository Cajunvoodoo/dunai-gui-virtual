-- |

module FRP.GUI.Utils where

import FRP.BearRiver
import Data.Text (Text)
import qualified Data.Text as T
import Data.Algorithm.Diff (getDiff, Diff, PolyDiff (..))


-- FIXME: use a list with better append asymptotics
-- TODO: write test case for holdList
holdList :: Monad m => Int -> SF m (Event a) [a]
holdList maxLen = proc a -> do
  (list, _) <- accumHoldBy accumFunc ([], 0) -< a
  returnA -< list
  where
    accumFunc :: ([a], Int) -> a -> ([a], Int)
    accumFunc (xs, n) x =
      if n >= maxLen
         then (tail xs ++ [x], n)
         else (xs ++ [x], n+1)

safeTail [] = []
safeTail xs = tail xs

tshow :: Show a => a -> Text
tshow = T.pack . show

diffText :: Text -> Text -> Event Text
diffText old new =
  wrapInEvent . T.pack . fmap fromDiff $ getDiff (T.unpack old) (T.unpack new)
  where
    fromDiff :: Diff a -> a
    fromDiff (First a)  = a
    fromDiff (Second a) = a
    fromDiff (Both a b) = b

    wrapInEvent :: Text -> Event Text
    wrapInEvent text =
      case text of
        "" -> NoEvent
        _  -> Event text
