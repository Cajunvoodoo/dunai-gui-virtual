-- | The abstract tree representation of the current (pure) GUI state.
-- Inspired by @gi-gtk-declarative@.

module FRP.GUI.State where

import Data.Unique
import FRP.GUI.Widgets
import Control.Monad (msum)
import FRP.GUI.Renderable
import Control.Monad.State.Lazy (MonadState, modify')
import Control.Monad.IO.Class (MonadIO (..))
import FRP.BearRiver (constM, SF)
import Control.Monad.State



-- type ID = Unique

-- data SomeTree where
--   SomeTree
--     :: Widget widget
--     => StateTree widget
--     -> SomeTree

-- -- | The @StateTree@ does not hold data on any particular widget representation,
-- -- unlike the version in gi-gtk-declarative. Instead, it serves to collect
-- -- the information about the layout of the GUI. The only information
-- -- it should hold is anything that ought to be given to the
-- -- backend, like changes made to that widget
-- data StateTree widget where
--   StateTreeWidget
--     :: ID
--     -> !(StateTreeNodeInfo widget)
--     -> StateTree widget

--   StateTreeBin
--     :: ID
--     -> !(StateTreeNodeInfo widget)
--     -> SomeTree
--     -> StateTree widget

--   StateTreeContainer
--     :: ID
--     -> !(StateTreeNodeInfo widget)
--     -> [SomeTree]
--     -> StateTree widget

-- data StateTreeNodeInfo widget
--   = StateTreeNodeInfo
--     {
--       -- | Changes made on the FRP side which must be reported
--       -- to the GUI side. Could also be the whole widget. Dynamics
--       -- are preferred here, since they can track both changes and
--       -- the current full state.
--       stateTreeNodeChanges :: [WidgetOutput widget]


--     -- Cajun: this shouldnt be necessary, as the reactimate argument for
--     -- network input will be these required updates

--     --   -- | Inputs made from the GUI side that must be accounted
--     --   -- for in the FRP side
--     -- , stateTreeNodeInputs  :: [WidgetInput  widget]
--     }

-- -- ** State Tree Operations
-- lookupST :: ID -> SomeTree -> Maybe SomeTree
-- lookupST id widget@(SomeTree (StateTreeWidget id' _)) =
--   if id == id'
--     then Just widget
--     else Nothing
-- lookupST id widget@(SomeTree (StateTreeBin id' _ child)) =
--   if id == id'
--     then Just widget
--     else lookupST id child
-- lookupST id widget@(SomeTree (StateTreeContainer id' _ childrens)) =
--   if id == id'
--     then Just widget
--     else msum $ lookupST id <$> childrens

-- appendTree :: SomeTree -> SomeTree -> SomeTree
-- appendTree child (SomeTree (StateTreeWidget _ _)) = error "cannot append to StateTreeWidget"
-- appendTree child (SomeTree (StateTreeBin id info _)) = SomeTree (StateTreeBin id info child)
-- appendTree child (SomeTree (StateTreeContainer id info children)) = SomeTree (StateTreeContainer id info (child:children))

-- appendState :: (MonadIO m, MonadState SomeTree m, Widget widget) => StateTreeNodeInfo widget -> m ()
-- appendState widgetInfo = do
--   unique <- liftIO newUnique
--   modify' (appendTree (SomeTree (StateTreeWidget unique widgetInfo)))

-- appendMSF :: Widget widget => StateTreeNodeInfo widget -> SF (StateT SomeTree IO) () ()
-- appendMSF = constM . appendState
