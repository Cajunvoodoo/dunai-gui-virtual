-- |

module FRP.GUI.GUI where

import FRP.BearRiver
import Control.Monad.State
import FRP.GUI.State
import FRP.GUI.Widgets
import Control.Monad.Trans.MSF (performOnFirstSample)

type GUI input output = SF (StateT SomeTree IO) input output


data Button =
  Button
    {
      toggled :: GUI (Event ()) Bool
    }

instance Widget Button where
  data WidgetInput Button = ButtonInput
  data WidgetOutput Button = ButtonOutput {btnOutput :: Event ()}

button :: SF (StateT SomeTree IO) (Event ()) Bool
button = proc ev -> do
  rec
    accumHoldBy (\b _ -> not b) False -< ev

-- foo = proc inp -> do
--   out <- button "blah" -< inp
