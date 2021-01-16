module MapGL.HTMLOverlay where

import Effect.Uncurried (EffectFn1)
import React as R
import WebMercator.LngLat (LngLat)
import WebMercator.Pixel (Pixel)

type RedrawArgs
  = { width :: Number
    , height :: Number
    , isDragging :: Boolean
    , project :: LngLat -> Pixel
    , unproject :: Pixel -> LngLat
    }

type Redraw
  = EffectFn1 RedrawArgs R.ReactElement

type Props
  = { redraw :: Redraw
    }

foreign import overlay :: R.ReactClass Props
