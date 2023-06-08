module FRP.GUI.Widgets where

import FRP.BearRiver
import Data.Kind (Type)

class Widget w where
  -- | The signals the widget can accept
  -- FIXME: markdown in haddock comments
  -- Ex. a textbox may accept Append, Delete, Clear, etc
  data WidgetInput w :: Type

  -- | The signals the widget can output. These should track
  -- the changes in the widget such that it renders correctly.
  -- Dynamics are often preferred, as they can hold both the
  -- change and the whole state.
  -- FIXME: markdown in haddock comments
  -- Ex. a textbox may output a @Dynamic@ containing its
  -- most recent addition and its current value
  data WidgetOutput w :: Type


--TODO: find a way to represent how it should be rendered
-- perhaps a typeclass function that acts in some monad
-- in order to maintain the state, like what changed and the current tree
