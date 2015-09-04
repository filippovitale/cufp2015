module Main where

import Prelude
import qualified Control.Monad.Aff as A
import Control.Monad.Aff.Console

import Halogen
import Halogen.Util
import qualified Halogen.HTML as H

data Input a = Click a

render :: forall p. Render Int Input p
render _ = H.div_ [ H.text "Hello world" ]

eval :: forall f. Eval Input Int Input f
eval (Click a) = pure a

ui :: forall f p. Component Int Input f p
ui = component render eval

main = A.launchAff do
  log "Hello sailor!"
