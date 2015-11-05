{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
import Graphics.VR.Pal
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Data.Foldable
import Control.Lens
import Halive.Utils
import Control.Monad.Trans
import Control.Monad.State
import Data.Maybe

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat) } 
  deriving Data

data Drag = Drag 
  { _drgPlayerStart :: Pose GLfloat
  , _drgHandStart   :: V3 GLfloat
  , _drgHandID      :: HandID 
  }
makeLenses ''Drag

data World = World
  { _wldPlayer :: Pose GLfloat
  , _wldDrag :: Maybe Drag
  }
makeLenses ''World

newWorld = World newPose Nothing

main :: IO ()
main = do
  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Grapple" NoGCPerFrame [UseHydra]

  shader       <- createShaderProgram "app/geo.vert" "app/geo.frag"
  useProgram shader
  Uniforms{..} <- acquireUniforms shader

  icoGeo     <- icosahedronGeometry 0.5 3
  icoShape   <- makeShape icoGeo shader

  cubeGeo    <- cubeGeometry (V3 0.3 0.3 1) 1
  cubeShape  <- makeShape cubeGeo shader

  let _ = [cubeShape, icoShape] :: [Shape Uniforms]
  glClearColor 0.01 0.01 0.05 1
  glEnable GL_DEPTH_TEST
  -- let view = viewMatrix (V3 0 0 5) (axisAngle (V3 0 1 0) 0)


  let n = 40
      each x z = mkTransformation 
        (axisAngle (V3 1 1 0) 0) 
        (V3 x (-3) z)
      grid = [each x z | 
                x <- [-n..n],
                z <- [-n..n]
                ]

  void . flip runStateT newWorld . whileWindow gpWindow $ do
    processEvents gpEvents (closeOnEscape gpWindow)

    (hands, handsType) <- getHands vrPal
    
    forM_ hands $ \hand -> do
      currentPlayer <- use wldPlayer
      drag          <- use wldDrag
      let handTranslation = hand ^. hndMatrix . translation
          handIsDragging  = hand ^. hndTrigger > 0.5
          handWasDragging = drag ^? traverse . drgHandID == Just (hand ^. hndID)

      when (handWasDragging && not handIsDragging) $ 
        wldDrag .= Nothing
      when (handIsDragging && not handWasDragging) $
        wldDrag ?= Drag
          { _drgHandID = hand ^. hndID
          , _drgHandStart = handTranslation
          , _drgPlayerStart = currentPlayer
          }

      when handIsDragging $ do
        mNewDrag <- use wldDrag
        forM_ mNewDrag $ \newDrag -> do
          let startPosition = newDrag ^. drgPlayerStart . posPosition
              handStart = newDrag ^. drgHandStart
          wldPlayer . posPosition .= startPosition + (handStart - handTranslation)
    
    view <- viewMatrixFromPose <$> use wldPlayer
    -- Draw the line
    renderWith vrPal view
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)) 
      $ \projection eyeView -> do
          let viewProj = projection !*! eyeView

          withShape icoShape $ do
            forM_ grid $ \model -> do
              uniformM44 uMVP (viewProj !*! model)
              drawShape
          
          withShape cubeShape $ do
            forM_ hands $ \hand -> do
              let model = hand ^. hndMatrix 

              uniformM44 uMVP (viewProj !*! model)
              drawShape