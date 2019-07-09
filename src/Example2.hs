{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Example2 where

import Prelude hiding (seq)

import Control.Monad

import Lens.Micro
import Lens.Micro.TH

import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.Pure.Game hiding (scale)

-------------------------------------
-- DSL Definition
-------------------------------------

-- duration in s
newtype Duration = For Float

-- end result value of animation
newtype To = To Float

data Ops obj a where
  Basic :: Duration -> Traversal' obj Float -> To -> Ops obj ()
  Create :: (obj -> (obj, Int)) -> Ops obj Int

data Dsl f a where
  Bind :: f a -> (a -> Dsl f b) -> Dsl f b
  Return :: a -> Dsl f a
  Par :: [Dsl f a] -> ([a] -> Dsl f b) -> Dsl f b

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

create :: (obj -> (obj, Int)) -> Dsl (Ops obj) Int
create create = Bind (Create create) (\index -> Return index)

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
applyOp obj t (Create create) = let
  (createdObj, objIndex) = create obj
  in (createdObj, Right objIndex)

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

data World
  = World
  { _sprites :: [Sprite]
  , _runningAnimations :: [Dsl (Ops World) ()]
  }

makeLenses ''World

-- Animation Definitions

createBox :: World -> (World, Int)
createBox w@(World {_sprites}) = let
  newIndex = w ^. sprites & length
  sprite = Sprite ((-120) + fromIntegral newIndex * 60) 0 0 30 boxPic
  newWorld = w { _sprites = _sprites ++ [sprite] }
  in (newWorld, newIndex)

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)

createBoxAnim :: Dsl (Ops World) ()
createBoxAnim = do
  i <- create createBox
  par
    [ basic (For 0.5) (sprites . atIndex i . alpha) (To 1)
    , basic (For 0.5) (sprites . atIndex i . scale) (To 20)
    ]

-- Gloss Functions

drawSprite :: Sprite -> Picture
drawSprite (Sprite {_x, _y, _alpha, _scale, _picture}) =
  _picture &
  Color (makeColor 1 1 1 _alpha) &
  Scale _scale _scale &
  Translate _x _y

draw :: World -> Picture
draw (World {_sprites}) = let
  worldSprites = _sprites
  in Pictures (map drawSprite worldSprites)

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'x') Down _ _) w@(World {_runningAnimations}) = let
  newRAnims = _runningAnimations ++ [createBoxAnim]
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
  worldSprites = []
  in World worldSprites []

main :: IO ()
main = let
  window = InWindow "animation-dsl" (400, 400) (100, 100)
  in play window black 60 initialWorld draw handleInput update

