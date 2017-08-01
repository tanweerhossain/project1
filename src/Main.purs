module Main where

import Prelude hiding (add)
import Control.Monad.Eff
import Data.Foreign
import Data.Foreign.Class
import Data.List
import Data.Maybe
import Prim as P
import Math as Math

import Mathbox.Classes as C
import Mathbox.Field
import Mathbox.Mathbox
import Mathbox.Types as T

matrixData = toForeign [ [ [-1, -1,  -1],[-1, -1,  -1],[ 1, -1,  -1],[ 1, -1,  -1]],[[-1, -1,  1],[-1, -1,  1],[ 1, -1,  1],[ 1, -1,  1]]]

mathbox :: MathboxPrimitive
mathbox =
  cartesian [
    cam,
    matrix,
    cube 0.9
  ]

cartesian :: Array MathboxPrimitive -> MathboxPrimitive
cartesian nested =
    (Cartesian $ C.mkCartesian { 
      range = Val [T.mkVec2 (-5) 5, T.mkVec2 (-5) 5, T.mkVec2 (-5) 5], 
      scale = Val (T.mkVec3 1 1 1)
    }) ( fromFoldable nested )

cam :: MathboxPrimitive
cam = Camera $ C.mkCamera { proxy = Val true, position = Just $ Val $ T.mkVec3 2 3 3 }

matrix :: MathboxPrimitive
matrix = Matrix $ C.mkMatrix { data = Just $ Val matrixData, channels = Val 3 }

surface :: String -> Number -> MathboxPrimitive
surface c o = Surface $ C.mkSurface { color = Val $ T.unsafeMkColor c, opacity = Val o }

transform :: T.Vec3 -> Array MathboxPrimitive -> MathboxPrimitive
transform v3 nested = (Transform3 $ C.mkTransform3 { position = Val $ v3 }) ( fromFoldable nested )

group :: Array MathboxPrimitive -> MathboxPrimitive
group nested = (Group $ C.mkGroup { active = Val true }) ( fromFoldable nested )

swizzle :: Array Int -> MathboxPrimitive
swizzle s = Swizzle $ C.mkSwizzle { order = Val $ T.mkSwizzle1 s }

cube :: Number -> MathboxPrimitive
cube opacity =  
        group [
          surface "blue" opacity,
          transform (T.mkVec3 0 2 0) [
            surface "blue" opacity,
            transform (T.mkVec3 0 (-2) 0) [
              swizzle [2, 3, 1],
              surface "red" opacity,
              transform (T.mkVec3 2 0 0) [
                surface "red" opacity,
                transform (T.mkVec3 (-2) 0 0) [
                  swizzle [3, 2, 1],
                  surface "yellow" opacity,
                  transform (T.mkVec3 0 0 2) [
                    surface "yellow" opacity
                  ]
                ]
              ]
            ]
          ]
        ]


main = do
  mkMathbox { plugins: ["core", "controls", "cursor"]
            , controls: { klass: trackballControls }
            , fullscreen: true
            } >>=
  applyOnThree (setThreeClearColor colorWhite 1.0) >>=
  set { focus: Just 0.3, scale: Just 72000.0 } >>=
  add (toJs mathbox)
