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
  wrapInEvent . T.pack . concatMap fromDiff $ getDiff (T.unpack old) (T.unpack new)
  where
    -- Ignore any differences that are strictly in the first string,
    -- as we consider it "static"
    fromDiff :: Diff Char -> String
    fromDiff (First a)  = ""
    fromDiff (Second a) = take 1 $ drop 1 $ show a
    fromDiff (Both a b) = ""

    wrapInEvent :: Text -> Event Text
    wrapInEvent text =
      case text of
        "\0" -> NoEvent
        _  -> Event text
