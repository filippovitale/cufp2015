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

render :: forall p. Render Int Input p
render n = H.div_ [
                    H.p_ [ H.text $ "Score: " ++ show n ],
                    H.button [ E.onClick $ E.input_ Click ]
                             [ H.text "Click me!" ]
                  ]

eval :: forall f. Eval Input Int Input f
-- eval has state Monad built in
eval (Click a) = do
  modify (+ 1)
  pure a
eval (Timer a) = do
  modify(`sub` 1)
  pure a
eval (GetScore f) = do
  n <- get
  pure $ f n

ui :: forall f p. Component Int Input f p
ui = component render eval

main = A.launchAff do
  app <- runUI ui 0
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
