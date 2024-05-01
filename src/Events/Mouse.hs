module Events.Mouse where

import Debug.Trace (trace)
import SDL
import GHC.Int (Int32)
import Data.List (foldl')

--type MouseState = Maybe (Point V2 Int32)
-- Bool indique si pressé ou non
-- Maybe (Point V2 Int32) indique la position du curseur
type MouseState = (Bool, Maybe (Point V2 Int32))

createMouseState :: MouseState
createMouseState = (False,Nothing)

handleEvents :: [Event] -> MouseState -> MouseState
handleEvents events ms = foldl' (flip handleEvent) ms events

handleEvent :: Event -> MouseState -> MouseState
handleEvent event ms@(pressed,_) =
    -- eventPayload : extraire les EventPayloads(keyboard,mouse...) de l'événement
  case eventPayload event of
    MouseMotionEvent mouseMotionEvent ->
      let pos = Just (mouseMotionEventPos mouseMotionEvent)
      in (pressed,pos)
    MouseButtonEvent mouseButtonEvent ->
      let pos = Just (mouseButtonEventPos mouseButtonEvent)
      in
        if mouseButtonEventMotion mouseButtonEvent == Pressed
        then
             (True,pos)
        else
             (False,pos)
    _ ->  ms
-- Debug.Trace.trace "Mouse:D'autre operation"


mousePressed :: MouseState -> Bool
mousePressed (pressed,_) = pressed

