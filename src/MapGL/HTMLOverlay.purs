module MapGL.HTMLOverlay where

import Control.Monad.Eff.Uncurried (EffFn1)
import React as R
import WebMercator.LngLat (LngLat)
import WebMercator.Pixel (Pixel)


type RedrawArgs =
  { width :: Number
  , height :: Number
  , isDragging :: Boolean
  , project :: LngLat -> Pixel
  , unproject :: Pixel -> LngLat
  }

type Redraw eff = EffFn1 eff RedrawArgs R.ReactElement

type Props eff =
  { redraw :: Redraw eff
  }

foreign import overlay :: forall eff. R.ReactClass (Props eff)
