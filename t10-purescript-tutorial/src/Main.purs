module Main where

import Prelude
import qualified Control.Monad.Aff as A
import Control.Monad.Aff.Console

import Halogen
import Halogen.Util
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

data Input a = Click a
             | Timer a
             | GetScore (Int -> a)
             -- free monad formulation of the actions

type Game = {
              score :: Int,
              high :: Int
            }

render :: forall p. Render Game Input p
render n = H.div_ [
                    H.p_ [ H.text $ "Score: " ++ show n.score ],
                    H.p_ [ H.text $ "High: " ++ show n.high ],
                    H.button [ E.onClick $ E.input_ Click ]
                             [ H.text "Click me!" ]
                  ]

click :: Game -> Game
click g = g { score = g.score + 1 }

timer :: Game -> Game
timer g = g { score = g.score -1 }

eval :: forall f. Eval Input Game Input f
-- eval has state Monad built in
eval (Click a) = do
  modify click
  pure a
eval (Timer a) = do
  modify timer
  pure a
eval (GetScore f) = do
  n <- gets _.score
  pure $ f n

ui :: forall f p. Component Game Input f p
ui = component render eval

main = A.launchAff do
  app <- runUI ui { score: 0, high: 0 }
  appendToBody app.node
  log "Hello sailor!"
  -- identation down here is IMPORTANT
  -- id ==== identity function
  -- the driver taked input unit
  let timer = do
        score <- app.driver $ GetScore id
        A.later' (300 - (score * 4)) do
          app.driver $ Timer unit
          timer
  timer
