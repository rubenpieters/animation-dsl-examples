{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Example1 where

import Prelude hiding (seq)

import Control.Monad

import Data.Either (fromLeft)

import Lens.Micro
import Lens.Micro.TH

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-------------------------------------
-- DSL Definition
-------------------------------------

-- duration in s
newtype Duration = For Float

-- end result value of animation
newtype To = To Float

data Ops obj a where
  Basic :: Duration -> Traversal' obj Float -> To -> Ops obj ()

instance Show (Ops obj a) where
  show (Basic (For duration) traversal (To x)) = "Basic (For " ++ show duration ++ ") (At " ++ "...) (To " ++ show x ++ ")"

data Dsl f a where
  Bind :: f a -> (a -> Dsl f b) -> Dsl f b
  Return :: a -> Dsl f a
  Par :: [Dsl f a] -> ([a] -> Dsl f b) -> Dsl f b

undefs :: [a]
undefs = undefined : undefs

instance (forall a. Show (f a)) => Show (Dsl f a) where
  show (Bind fa k) = "Bind (" ++ show fa ++ ") (" ++ (show (k undefined)) ++ ")"
  show (Par fs k) = "Par (" ++ show fs ++ ") (" ++ (show (k undefs)) ++ ")"
  show (Return a) = "Return"

instance Functor (Dsl f) where
  fmap = liftM

instance Applicative (Dsl f) where
  pure = return
  (<*>) = ap

instance Monad (Dsl f) where
  return = Return
  (Bind fa k) >>= k2 = Bind fa (\x -> k x >>= k2)
  (Par fs k) >>= k2 = Par fs (\l -> k l >>= k2)
  (Return a) >>= k2 = k2 a

seq :: [Dsl (Ops obj) ()] -> Dsl (Ops obj) ()
seq [] = Return ()
seq (anim:r) = anim >>= (\_ -> seq r)

basic :: Duration -> Traversal' obj Float -> To -> Dsl (Ops obj) ()
basic duration traversal to = Bind (Basic duration traversal to) (\_ -> Return ())

par :: [Dsl (Ops obj) ()] -> Dsl (Ops obj) ()
par l = Par l (\_ -> Return ())

-------------------------------------
-- Apply Functions
-------------------------------------

-- Apply Operation

applyOp :: obj -> Float -> Ops obj a -> (obj, Either (Ops obj a) a)
applyOp obj t (Basic (For duration) traversal (To x)) = let
  -- (1) update world model values
  newObj = obj & traversal %~ updateValue t duration x
  -- (2) reduce duration
  newDuration = duration - t
  -- (3) create new animation/result
  result = if newDuration > 0
    then Left (Basic (For newDuration) traversal (To x))
    else Right ()
  in (newObj, result)

updateValue ::
  Float -> -- time elapsed
  Float -> -- duration
  Float -> -- target value
  Float -> -- current value
  Float -- new value
updateValue t duration target current = let
  newValue = (current + ((target - current) * t) / duration)
  in if target > current
    then min target newValue
    else max target newValue

-- Example 1

example1_state :: (Float, Float)
example1_state = (100, 100)

example1_anim :: Ops (Float, Float) ()
example1_anim = Basic (For 0.2) _1 (To 150)

example1_1 :: ((Float, Float), Either (Ops (Float, Float) ()) ())
example1_1 = applyOp example1_state 0.1 example1_anim

example1_2 :: ((Float, Float), Either (Ops (Float, Float) ()) ())
example1_2 = applyOp (fst example1_1) 0.1 (fromLeft undefined (snd example1_1))

-- Apply DSL

applyDsl :: obj -> Float -> Dsl (Ops obj) a -> (obj, Dsl (Ops obj) a)
-- (1) Bind case
applyDsl obj t (Bind fa k) = let
  (newObj, eResult) = applyOp obj t fa
  in case eResult of
    Right result -> (newObj, k result)
    Left newOp -> (newObj, Bind newOp k)
-- (2) Par case
applyDsl obj t (Par fs k) = let
  (newObj, newOps) = applyOps obj t fs
  in case returnValues newOps of
    Right l -> (newObj, k l)
    Left () -> (newObj, Par newOps k)
-- (3) Return case
applyDsl obj t (Return a) = (obj, Return a)

applyOps :: obj -> Float -> [Dsl (Ops obj) a] -> (obj, [Dsl (Ops obj) a])
applyOps obj t [] = (obj, [])
applyOps obj t (op:r) = let
  -- apply first operation
  (obj', op') = applyDsl obj t op
  -- apply the rest of the operations
  (obj'', ops) = applyOps obj' t r
  in (obj'', op' : ops)

returnValues :: [Dsl f a] -> Either () [a]
returnValues [] = Right []
returnValues ((Return a):r) = do
  l <- returnValues r
  return (a : l)
returnValues _ = Left ()

-- Example 2

example2_state :: (Float, Float)
example2_state = (100, 100)

example2_anim :: Dsl (Ops (Float, Float)) ()
example2_anim =
  par
  [ basic (For 0.2) _1 (To 150)
  , basic (For 0.2) _2 (To 150)
  ]

example2_1 :: ((Float, Float), Dsl (Ops (Float, Float)) ())
example2_1 = applyDsl example2_state 0.1 example2_anim

example2_2 :: ((Float, Float), Dsl (Ops (Float, Float)) ())
example2_2 = applyDsl (fst example2_1) 0.1 (snd example2_1)

-------------------------------------
-- Gluing With Gloss
-------------------------------------

-- World Definition

data Sprite
  = Sprite
  { _x :: Float
  , _y :: Float
  , _alpha :: Float
  , _scale :: Float
  , _picture :: Picture
  }

makeLenses ''Sprite

data Sprites
  = Sprites
  { _box :: Sprite
  }

makeLenses ''Sprites

allSprites :: Sprites -> [Sprite]
allSprites (Sprites box) = [box]

data World
  = World
  { _sprites :: Sprites
  , _runningAnimations :: [Dsl (Ops World) ()]
  }

makeLenses ''World

-- Animation Definitions

basicBoxAnim = basic (For 0.5) (sprites . box . x) (To 50)

seqBoxAnim = seq
  [ basic (For 0.5) (sprites . box . x) (To 50)
  , basic (For 0.5) (sprites . box . y) (To 50)
  , basic (For 0.5) (sprites . box . x) (To 0)
  , basic (For 0.5) (sprites . box . y) (To 0)
  ]

seqBoxAnim' =
  Bind (Basic (For 0.5) (sprites . box . x) (To 50)) $ \_ ->
  Bind (Basic (For 0.5) (sprites . box . y) (To 50)) $ \_ ->
  Bind (Basic (For 0.5) (sprites . box . x) (To 0)) $ \_ ->
  Bind (Basic (For 0.5) (sprites . box . y) (To 0)) $ \_ ->
  Return ()

parBoxAnim = par
  [ basic (For 0.5) (sprites . box . x) (To 50)
  , basic (For 0.5) (sprites . box . y) (To 50)
  ]

parBoxAnim' =
  Par
  [ Bind (Basic (For 0.2) (sprites . box . x) (To 150)) (\_ -> Return ())
  , Bind (Basic (For 0.2) (sprites . box . y) (To 50)) (\_ -> Return ())
  ] $ \_ ->
  Return ()

fancyBoxAnim = let
  fade = seq
    [ basic (For 0.125) (sprites . box . alpha) (To 0)
    , basic (For 0.125) (sprites . box . alpha) (To 1)
    ]
  in par
  [ seqBoxAnim
  , seq (replicate 8 fade)
  ]

boxAnim :: Dsl (Ops World) ()
boxAnim = fancyBoxAnim

-- Gloss Functions

drawSprite :: Sprite -> Picture
drawSprite (Sprite {_x, _y, _alpha, _scale, _picture}) =
  _picture &
  Color (makeColor 1 1 1 _alpha) &
  Scale _scale _scale &
  Translate _x _y

draw :: World -> Picture
draw (World {_sprites}) = let
  worldSprites = allSprites _sprites
  in Pictures (map drawSprite worldSprites)

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'x') Down _ _) w@(World {_runningAnimations}) = let
  newRAnims = _runningAnimations ++ [boxAnim]
  in w { _runningAnimations = newRAnims }
handleInput _ w = w

update :: Float -> World -> World
update t w@(World {_runningAnimations}) = let
  (newWorld, newOps) = applyOps w t _runningAnimations
  f (Return _) = False
  f _ = True
  filteredOps = filter f newOps
  in newWorld { _runningAnimations = filteredOps }

boxPic :: Picture
boxPic = Pictures
  [ Line [(-1, 1), (1, 1)]
  , Line [(1, 1), (1, -1)]
  , Line [(1, -1), (-1, -1)]
  , Line [(-1, -1), (-1, 1)]
  ]

initialWorld :: World
initialWorld = let
  worldSprites = Sprites (Sprite 0 0 1 20 boxPic)
  in World worldSprites []

main :: IO ()
main = let
  window = InWindow "animation-dsl" (400, 400) (100, 100)
  in play window black 60 initialWorld draw handleInput update
