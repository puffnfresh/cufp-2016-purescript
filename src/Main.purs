module Main where

import Pux (Update, App, start, noEffects, renderToDOM)
import Pux.Html as H
import Control.Apply (lift2)
import Control.Monad.Aff (launchAff, later')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Foldable (traverse_)
import Data.List (List(Nil), (:))
import Data.Tuple (Tuple(Tuple))
import Pux.Html.Events (onClick)
import Signal (runSignal, sampleOn)
import Signal.Channel (subscribe, send, CHANNEL)
import Prelude hiding (div)

-- covariant :: (a -> b)         -> f a -> f b
-- applicative :: f (a -> b)     -> f a -> f b
-- monad :: (a -> f b)           -> f a -> f b
-- contravariant :: (b -> a)     -> f a -> f b
-- invariant :: (a -> b, b -> a) -> f a -> f b
--
-- profunctor :: (b -> a, c -> d) -> f a c -> f b d
-- bifunctor :: (a -> b, c -> d)  -> f a c -> f b d
--
-- class Functor f => Applicative f where
--
-- class Applicative f => Monad f where
--   bind :: (a -> f b) -> f a -> f b
--
-- instance Functor Maybe where
--   map f (Just a) = Just (f a)
--   -- Nothing
--
-- instance Monad Maybe where
--   bind f (Just a) = f a
--   bind f Nothing = Nothing
--
--
-- zip :: forall f. (Applicative f) => f a -> f b -> f (Tuple a b)
-- join :: forall f. (Monad f) => f (f a) -> f a
--
-- -- Kleisli
-- compose :: forall f a b c. (Monad f) => (a -> f b) ->
--                                         (b -> f c) ->
--                                         (a -> f c)
--
--   -- Maybe (Maybe Int) -> Maybe Int
--   -- Just Nothing = Nothing
--   -- Just (Just 1) = Just 1
--   -- Nothing = Nothing
--
-- example :: forall f a. (Functor f) => f a -> f Unit
-- example eff = map (\_ -> unit) eff
--
-- doTwice :: forall f a. (Monad f) => f a -> f a
-- doTwice fa = bind (\a -> fa) fa
--
-- forever :: forall f a. (Monad f) => f a -> f a
-- forever fa = do
--   fa
--   forever fa
--   -- bind (\a -> forever fa) fa
--
-- class Contravariant f where
--   contramap :: (b -> a) -> f a -> f b

foreign import sizeBar
  :: forall e. State -> Eff (dom :: DOM | e) Unit

data Input
  = Click
  | Tick
  | None

type State
  = { current :: Int, high :: Int }

view :: State -> H.Html Input
view n =
  H.div [ ] [ H.div [ ] [ H.text (show n.current) ]
            , H.div [ ] [ H.text (show n.high) ]
            , H.button [ onClick (\_ -> Click) ]
                       [ H.text "Click me" ]
            ]

update :: forall e. Update State Input (dom :: DOM | e)
update Click n =
  let newScore = n.current + 1
      newState = n { current = newScore
                 , high = max newScore n.high
                 }
  in { state: newState
     , effects: [ liftEff (sizeBar newState) $> None ]
     }
update Tick n =
  let newState = n { current = max (n.current - 1) 0 }
  in { state: newState
     , effects: [ liftEff (sizeBar newState) $> None ]
     } -- {state: n - 1, effects: []}
update None n =
  noEffects n

-- Signal action -> Signal state -> Signal (Tuple action state)

tupled :: forall f a b. (Apply f) => f a -> f b -> f (Tuple a b)
tupled = lift2 Tuple

onTick :: forall e. State -> App State Input -> Input -> Eff (channel :: CHANNEL, err :: EXCEPTION | e) Unit
onTick s app Tick =
  void (launchAff (later' (300 - (4 * s.current))
    (liftEff (send app.actionChannel (Tick : Nil)))))
onTick _ _ _ = pure unit

onActionState :: forall e. App State Input -> Tuple (List Input) State -> Eff (channel :: CHANNEL, err :: EXCEPTION | e) Unit
onActionState app  (Tuple actions state) =
  traverse_ (onTick state app) actions

main :: forall e. Eff (dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  let initialState = { current: 0, high: 0 }
  app <- start {
    update: update,
    view: view,
    initialState: initialState,
    inputs: [ ]
  }
  let actions = subscribe app.actionChannel
      actionStates = sampleOn actions (tupled actions app.state)
  runSignal (map (onActionState app) actionStates)
  onTick initialState app Tick
  sizeBar { current: 10, high: 100 }
  renderToDOM "#app" app.html
