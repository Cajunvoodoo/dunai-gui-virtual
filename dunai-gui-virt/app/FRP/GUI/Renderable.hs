-- | TODO: summary of Renderable module

module FRP.GUI.Renderable where
import Data.Kind (Type)
import Control.Monad.IO.Class

-- | Converts an abstract widget into its backend representation.
-- This is to be implemented by the backend for each
class Renderable frontW backW where
  -- | The output of the rendered widget.
  -- type family RenderType widget :: Type

  -- | Turn the abstract widget definition (FRP side) into
  -- its backend-specific representation
  render
    :: MonadIO m
    => frontW
    -> m backW
