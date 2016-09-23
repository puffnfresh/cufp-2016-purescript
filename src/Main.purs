module Main where

import Pux.Html as H
import Control.Monad.Aff (launchAff, later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (traverse_)
import Data.List ((:), List(Nil))
import Data.Tuple (Tuple(Tuple))
import Pux (App, renderToDOM, Update, start)
import Pux.Html.Events (onClick)
import Signal (map2, sampleOn, runSignal)
import Signal.Channel (subscribe, send, CHANNEL)
import Prelude hiding (div)

data Input = Click
           | Tick

type State = { score :: Int, high :: Int }

update :: forall e. Update State Input e
update Click state =
  let score = state.score + 1
  in { state: { score: score, high: max score state.high }, effects: [ ] }
update Tick state =
  let score = max 0 (state.score - 1)
  in { state: state { score = score }, effects: [ ] }

view :: State -> H.Html Input
view state =
  let score = H.div [ ] [ H.text "Score: ",
                          H.text (show state.score)
                        ]
      high = H.div [ ] [ H.text "High: ", H.text (show state.high) ]
  in H.div [] [ score
              , high
              , H.button [ onClick (\e -> Click) ] [ H.text "Click me" ]
              ]

tick :: forall e. App State Input -> Eff (channel :: CHANNEL | e) Unit
tick app = send app.actionChannel (Tick:Nil)

onTick :: forall e. App State Input -> Input -> State -> Eff (console :: CONSOLE, channel :: CHANNEL, err :: EXCEPTION | e) Unit
onTick app Tick state =
  let e = liftEff $ tick app
  in do
    log "TICKED"
    void (launchAff (later' (300 - (4 * state.score)) e))
onTick _ _ _ = pure unit

sendTick :: forall e. App State Input -> Tuple (List Input) State -> Eff (console :: CONSOLE, channel :: CHANNEL, err :: EXCEPTION | e) Unit
sendTick app (Tuple inputs state) = traverse_ (\i -> onTick app i state) inputs

main :: forall e. Eff (channel :: CHANNEL, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  app <- start {
    update: update,
    view: view,
    initialState: { score: 0, high: 0 },
    inputs: [ ]
  }
  let inputs = subscribe app.actionChannel
      inputStates = map2 Tuple inputs app.state
      onStates = sampleOn inputs inputStates
  tick app
  runSignal $ map (sendTick app) onStates
  renderToDOM "#app" app.html
  log "Hello sailor!"
