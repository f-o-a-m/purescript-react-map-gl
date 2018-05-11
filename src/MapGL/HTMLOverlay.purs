module MapGL.HTMLOverlay where

import Control.Monad.Eff (Eff)
import React as R
import WebMercator.Pixel (Pixel)
import WebMercator.LngLat (LngLat)


type RedrawArgs =
  { width :: Number
  , height :: Number
  , isDragging :: Boolean
  , project :: LngLat -> Pixel
  , unproject :: Pixel -> LngLat
  }

type Props eff =
  { redraw :: RedrawArgs -> Eff eff R.ReactElement
  }

foreign import overlay :: forall eff. R.ReactClass (Props eff)
