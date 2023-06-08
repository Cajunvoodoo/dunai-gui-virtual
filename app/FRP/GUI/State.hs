-- | The abstract tree representation of the current (pure) GUI state.
-- Inspired by @gi-gtk-declarative@.

module FRP.GUI.State where

import Data.Unique (Unique)
import FRP.GUI.Widgets

type ID = Unique

data SomeTree where
  SomeTree
    :: Widget widget
    => StateTree widget child
    -> SomeTree

-- | The @StateTree@ does not hold data on any particular widget representation,
-- unlike the version in gi-gtk-declarative. Instead, it serves to collect
-- the information about the layout of the GUI. The only information
-- it should hold is anything that ought to be given to the
-- backend, like changes made to that widget
data StateTree widget child where
  StateTreeWidget
    :: ID
    --FIXME: style stuff goes here?
    -> !(StateTreeNodeInfo widget child)
    -> StateTree widget child

  StateTreeBin
    :: ID
    -> !(StateTreeNodeInfo widget child)
    -> SomeTree
    -> StateTree widget child

  StateTreeContainer
    :: ID
    -> !(StateTreeNodeInfo widget child)
    -> [SomeTree]
    -> StateTree widget child


data StateTreeNodeInfo widget child
  = StateTreeNodeInfo
    {
      -- | Changes made on the FRP side which must be reported
      -- to the GUI side. Could also be the whole widget. Dynamics
      -- are preferred here, since they can track both changes and
      -- the current full state.
      stateTreeNodeChanges :: [WidgetOutput widget]

    -- Cajun: this shouldnt be necessary, as the reactimate argument for
    -- network input will be these required updates

    --   -- | Inputs made from the GUI side that must be accounted
    --   -- for in the FRP side
    -- , stateTreeNodeInputs  :: [WidgetInput  widget]
    }
