module Tracer.DemoScene (demoScene) where

import           Data.Vect
import           Tracer
import           Tracer.Shapes
import           Tracer.Lights

baseMaterial :: Material
baseMaterial = Material {
  mShininess = 100,
  mSpec = Color 1 1 1,
  mDiff = Color 1 1 1,
  mAmb = 0.1
}

planeMaterial :: Material
planeMaterial = baseMaterial { mSpec = Color 1 1 1 }

demoScene :: Scene
demoScene = Scene {
  bgColor = Color 0 0 0
  , objects =
      [ Ob { material = planeMaterial
           , shape = Plane (Vec3 0 (-3) 0) (Vec3 0 1 0)
           }
      --, Ob { material = planeMaterial
      --     , shape = Plane (Vec3 0 3 0) (Vec3 0 (-1) 0)
      --     }
      , Ob { material = planeMaterial
           , shape = Plane (Vec3 0 0 15) (Vec3 0 0 (-1))
           }
      , Ob { material = planeMaterial
           , shape = Plane (Vec3 (-3) 0 0) (Vec3 1 0 0)
           }
      , Ob { material = planeMaterial
           , shape = Plane (Vec3 (3) 0 0) (Vec3 1 0 0)
           }
      , Ob { material = baseMaterial { mDiff = Color 0 0 0.8 }
           , shape = Triangle (Vec3 0 0 10) (Vec3 0 1 10) (Vec3 1 0 10)
           }
      , Ob { material = baseMaterial { mDiff = Color 0.8 0 0 }
           , shape = Sphere (Vec3 (-0.5) (-0.5) 10.5) 1
           }
      ]
  , lights =  [ AnyLight $ mkDirLight (Vec3 0 (-1) 0) (Color 0.3 0.3 0.3)
              , AnyLight $ PointLight (Vec3 (-2) 1 10) (Color 3 3 3)
              , AnyLight $ PointLight (Vec3 2 0 10) (Color 3 1.5 0)
              ]
  }