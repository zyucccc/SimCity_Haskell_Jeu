
module Model where

import SDL

import GHC.Int (Int32)

import Events.Keyboard (Keyboard)
import qualified Events.Keyboard as K

import Events.Mouse (MouseState)
import qualified Events.Mouse as MOS

import Debug.Trace (trace)


data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int
                           , mouseClick :: MouseState
                           , displayText :: Maybe String}
-- }
  deriving (Show)


initGameState :: GameState
initGameState = GameState 200 300 4 Nothing Nothing

moveLeft :: GameState -> GameState
moveLeft gs = Debug.Trace.trace "Keyboard: Move left" gs { persoX = persoX gs - speed gs }

moveRight :: GameState -> GameState
moveRight gs =Debug.Trace.trace "Keyboard: Move Right"  gs { persoX = persoX gs + speed gs }
                              
moveUp :: GameState -> GameState
moveUp gs = Debug.Trace.trace "Keyboard: Move Up"  gs  { persoY = persoY gs - speed gs }

moveDown :: GameState -> GameState
moveDown gs = Debug.Trace.trace "Keyboard: Move Down"  gs { persoY = persoY gs + speed gs }

gameStep :: RealFrac a => GameState -> Keyboard -> MouseState -> a -> GameState
gameStep gstate kbd mos deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)
              .
              (if MOS.mousePressed mos
               then handleMouseClick else id)
               in modif gstate

 


handleMouseClick :: GameState -> GameState
handleMouseClick gs = 
  let mos = mouseClick gs
      px = fromIntegral $ persoX gs
      py = fromIntegral $ persoY gs
  in case mos of
      --  Just _ -> Debug.Trace.trace "Mouse click!" gs
       Just (P (V2 x y)) -> 
        if x > px  && x < px + 100 && y > py  && y < py + 100
        then gs { displayText = Just "Touché!" }
        else gs
       Nothing -> gs
-- Debug.Trace.trace "Touché!"