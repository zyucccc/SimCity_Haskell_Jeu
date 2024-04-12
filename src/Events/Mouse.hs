module Events.Mouse where

import Debug.Trace (trace)
import SDL
import GHC.Int (Int32)
import Data.List (foldl')

type MouseState = Maybe (Point V2 Int32) 

createMouseState :: MouseState
createMouseState = Nothing

handleEvents :: [Event] -> MouseState -> MouseState
handleEvents events ms = foldl' (flip handleEvent) ms events

handleEvent :: Event -> MouseState -> MouseState
handleEvent event ms =
    -- eventPayload : extraire les EventPayloads(keyboard,mouse...) de l'événement
  case eventPayload event of
    -- MouseMotionEvent mouseMotionEvent ->
    --     -- mouseMotionEventPos : accept mouseMotionEvent and return the position of the mouse
    --   Debug.Trace.trace "Pressed-Motion!"
    --   Just (mouseMotionEventPos mouseMotionEvent)
    MouseButtonEvent mouseButtonEvent ->
      if mouseButtonEventMotion mouseButtonEvent == Pressed
      then 
        Debug.Trace.trace "Pressed-press!"
        Just (mouseButtonEventPos mouseButtonEvent)
      else 
        Debug.Trace.trace "Pressed-release!"
        Nothing
    _ ->  ms
-- Debug.Trace.trace "Mouse:D'autre operation"


mousePressed :: MouseState -> Bool
mousePressed ms = case ms of
  Just _ -> Debug.Trace.trace "mousePressed!" True
  Nothing -> Debug.Trace.trace "mouseVide " False
