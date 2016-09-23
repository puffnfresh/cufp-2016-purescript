module Main where

import Pux.Html as H
import Control.Monad.Aff (launchAff, later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Pux (renderToDOM, Update, start)
import Pux.Html.Events (onClick)
import Signal.Channel (CHANNEL)
import Prelude hiding (div)

data Input = Click
           | Tick

update :: forall e. Update Int Input e
update Click state = { state: state + 1, effects: [ ] }
update Tick state = { state: state - 1, effects: [ ] }

view :: Int -> H.Html Input
view score = H.div [ ] [ H.text "Score: ",
                         H.text (show score),
                         H.button [ onClick (\e -> Click) ] [ H.text "Click me" ]
                       ]

main :: forall e. Eff (channel :: CHANNEL, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  app <- start {
    update: update,
    view: view,
    initialState: 0,
    inputs: [ ]
  }
  launchAff $ later' 300 do
    liftEff $ log "300ms"
  renderToDOM "#app" app.html
  log "Hello sailor!"
