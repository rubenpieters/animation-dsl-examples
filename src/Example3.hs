{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Example3 where

import Prelude hiding (seq)

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find, findIndex)

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.Pure.Game hiding (scale)

import Debug.Trace

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
  Delete :: (Int -> obj -> obj) -> Int -> Ops obj ()

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

delete :: (Int -> obj -> obj) -> Int -> Dsl (Ops obj) ()
delete f id = Bind (Delete f id) (\_ -> Return ())

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
applyOp obj t (Delete f id) =
  (f id obj, Right ())

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

cleanAnims :: [Dsl (Ops obj) a] -> [Dsl (Ops obj) a]
cleanAnims l = let
  f (Return _) = False
  f _ = True
  in filter f l

-------------------------------------
-- Game Code
-------------------------------------

-- Picture Definitions

trianglePic :: Picture
trianglePic = lineLoop [(0, 1), (1, -1), (-1, -1)]

circlePic :: Picture
circlePic = ThickCircle 0.3 15

particlePic :: Picture
particlePic = Circle 5

bulletPic :: Picture
bulletPic = circleSolid 1.5

-- World Definition

data Sprite
  = Sprite
  { _x :: Float
  , _y :: Float
  , _alpha :: Float
  , _color :: (Float, Float, Float)
  , _scale :: Float
  , _rotation :: Float -- degrees
  , _picture :: Picture
  , _spriteId :: Int
  }

makeLenses ''Sprite

data Player
  = Player
  { _playerSprite :: Sprite
  , _shootCD :: Float
  }

makeLenses ''Player

data Enemy
  = Enemy
  { _enemySprite :: Sprite
  , _enemyHp :: Float
  }

makeLenses ''Enemy

data Bullet
  = Bullet
  { _bulletSprite :: Sprite
  , _bulletDirection :: (Float, Float)
  }

makeLenses ''Bullet

data World
  = World
  { _player :: Player
  , _bullets :: [Bullet]
  , _enemy :: Enemy
  , _keysDown :: Set Key
  , _mouse :: (Float, Float)
  , _particles :: [Sprite]
  , _nextParticleId :: Int
  , _particleAnims :: [Dsl (Ops World) ()]
  }

makeLenses ''World

allSprites :: World -> [Sprite]
allSprites w =
  [w ^. player . playerSprite] ++
  map _bulletSprite (w ^. bullets) ++
  [w ^. enemy . enemySprite] ++
  w ^. particles

atIndex :: Int -> Lens' [a] a
atIndex i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)

paId :: Int -> Lens' [Sprite] Sprite
paId i = let
  get l = case find (\x -> x ^. spriteId == i) l of
    Just s -> s
    Nothing -> error ("no particle with index " ++ show i)
  set l x = case findIndex (\x -> x ^. spriteId == i) l of
    Just ix -> take ix l ++ x : drop (ix+1) l
    Nothing -> error ("no particle with index " ++ show i)
  in lens get set

-- Config

playerMoveSpeed :: Float
playerMoveSpeed = 2

bulletSpeed :: Float
bulletSpeed = 6

mkBullet :: Float -> Float -> (Float, Float) -> Bullet
mkBullet x y dir = Bullet
  (Sprite x y 1 (0.2, 0.3, 0.85) 4 0 bulletPic undefined)
  dir

createParticle :: (Float, Float) -> World -> (World, Int)
createParticle (x, y) w@(World {_particles, _nextParticleId}) = let
  newIndex = w ^. nextParticleId
  particle = Sprite x y 1 (0.7, 0.35, 0.5) 1 0 particlePic newIndex
  newWorld = w { _particles = particle : _particles, _nextParticleId = _nextParticleId + 1 }
  in (newWorld, newIndex)

deleteParticle :: Int -> World -> World
deleteParticle id w@(World {_particles}) = let
  newWorld = w { _particles = filter (\x -> x ^. spriteId /= id) _particles }
  in newWorld

enemyHitAnim :: (Float, Float) -> Dsl (Ops World) ()
enemyHitAnim pos@(paX, paY) = let
  paDur = 0.5
  in par
  [ do
      i <- create (createParticle pos)
      par
        [ basic (For paDur) (particles . paId i . x) (To (paX + 1 * 10))
        , basic (For paDur) (particles . paId i . y) (To (paY + 1 * 10))
        ]
      delete deleteParticle i
  , do
      i <- create (createParticle pos)
      par
        [ basic (For paDur) (particles . paId i . x) (To (paX + (-1) * 10))
        , basic (For paDur) (particles . paId i . y) (To (paY + (-1) * 10))
        ]
      delete deleteParticle i
  , do
      i <- create (createParticle pos)
      par
        [ basic (For paDur) (particles . paId i . x) (To (paX + 1 * 10))
        , basic (For paDur) (particles . paId i . y) (To (paY + (-1) * 10))
        ]
      delete deleteParticle i
  , do
      i <- create (createParticle pos)
      par
        [ basic (For paDur) (particles . paId i . x) (To (paX + (-1) * 10))
        , basic (For paDur) (particles . paId i . y) (To (paY + 1 * 10))
        ]
      delete deleteParticle i
  ]

-- Initial Definitions

initialPlayer :: Player
initialPlayer = Player
  (Sprite 0 0 1 (1, 1, 1) 10 0 trianglePic undefined)
  0

initialEnemy :: Enemy
initialEnemy = Enemy
  (Sprite 0 100 1 (0.8, 0.2, 0.2) 5 0 circlePic undefined)
  100

initialWorld :: World
initialWorld = World initialPlayer [] initialEnemy Set.empty (0, 0) [] 1 []

-- Gloss Functions

overlapEn :: Bullet -> Enemy -> Bool
overlapEn bullet enemy = let
  eX = enemy ^. enemySprite . x
  eY = enemy ^. enemySprite . y
  bX = bullet ^. bulletSprite . x
  bY = bullet ^. bulletSprite . y
  dist = sqrt ((bX - eX) * (bX - eX) + (bY - eY) * (bY - eY))
  in dist <= 50

outsideBounds :: Bullet -> Bool
outsideBounds bullet = let
  bX = bullet ^. bulletSprite . x
  bY = bullet ^. bulletSprite . y
  in bX <= -800 || bX >= 800 || bY <= -800 || bY >= 800

drawSprite :: Sprite -> Picture
drawSprite (Sprite {_x, _y, _alpha, _color, _scale, _rotation, _picture}) =
  _picture &
  Rotate _rotation &
  Color (makeColor (_color ^. _1) (_color ^. _2) (_color ^. _3) _alpha) &
  Scale _scale _scale &
  Translate _x _y

draw :: World -> Picture
draw w = let
  worldSprites = allSprites w
  in Pictures (map drawSprite worldSprites)

handleInput :: Event -> World -> World
handleInput (EventKey k Down _ _) = execState $ do
  keysDown %= \x -> Set.insert k x
handleInput (EventKey k Up _ _) = execState $ do
  keysDown %= \x -> Set.delete k x
handleInput (EventMotion mousePos) = execState $ do
  mouse .= mousePos
handleInput _ = id

reorient :: World -> World
reorient = execState $ do
  pX <- use $ player . playerSprite . x
  pY <- use $ player . playerSprite . y
  mX <- use $ mouse . _1
  mY <- use $ mouse . _2
  let -- calculate mouse pos relative to player pos
      rMX = mX - pX
      rMY = mY - pY
      -- calculate angle
      norm = sqrt (rMX * rMX + rMY * rMY)
      angle = if norm == 0 then 0 else acos (rMY / norm)
      angleDeg = angle * 180 / pi
  if mX < pX
    then player . playerSprite . rotation .= - angleDeg
    else player . playerSprite . rotation .= angleDeg

update :: Float -> World -> World
update t = execState $ do
  keys <- use keysDown
  -- player movement
  if Set.member (Char 'w') keys
    then player . playerSprite . y %= \x -> x + playerMoveSpeed
    else return ()
  if Set.member (Char 'a') keys
    then player . playerSprite . x %= \x -> x - playerMoveSpeed
    else return ()
  if Set.member (Char 's') keys
    then player . playerSprite . y %= \x -> x - playerMoveSpeed
    else return ()
  if Set.member (Char 'd') keys
    then player . playerSprite . x %= \x -> x + playerMoveSpeed
    else return ()
  -- player shooting
  player . shootCD %= \x -> x - 1
  cd <- use $ player . shootCD
  if Set.member (SpecialKey KeySpace) keys && cd <= 0
    then do
      playerX <- use $ player . playerSprite . x
      playerY <- use $ player . playerSprite . y
      mouseX <- use $ mouse . _1
      mouseY <- use $ mouse . _2
      let -- calculate mouse pos relative to player pos
          rMX = mouseX - playerX
          rMY = mouseY - playerY
          -- normalize
          norm = sqrt (rMX * rMX + rMY * rMY)
          nMX = if norm == 0 then 0 else rMX / norm
          nMY = if norm == 0 then 1 else rMY / norm
      bullets %= \x -> mkBullet playerX playerY (nMX, nMY) : x
      player . shootCD .= 20
    else return ()
  -- reorient player
  modify reorient
  -- update bullet pos
  w <- get
  let (newOps, newBullets) = updateBullets w (w ^. bullets)
  put (w { _particleAnims = newOps ++ w ^. particleAnims, _bullets = newBullets})
  -- update particle animations
  w <- get
  let (newWorld, newOps) = applyOps w t (w ^. particleAnims)
      cleanedOps = cleanAnims newOps
  put (newWorld { _particleAnims = cleanedOps })

updateBullets :: World -> [Bullet] -> ([Dsl (Ops World) ()], [Bullet])
updateBullets w [] = ([], [])
updateBullets w (bullet:r) = let
  bX = bullet ^. bulletSprite . x
  bY = bullet ^. bulletSprite . y
  en = w ^. enemy
  newBullet = updateBullet bullet
  (newAnims, newList) = updateBullets w r
  in if outsideBounds newBullet
       then (newAnims, newList)
       else if overlapEn newBullet en
              then (enemyHitAnim (bX, bY) : newAnims, newList)
              else (newAnims, newBullet : newList)

updateBullet :: Bullet -> Bullet
updateBullet bullet = let
  (dirX, dirY) = bullet ^. bulletDirection
  in bullet & bulletSprite . x %~ (\x -> x + bulletSpeed * dirX)
            & bulletSprite . y %~ (\x -> x + bulletSpeed * dirY)

main :: IO ()
main = let
  window = InWindow "animation-dsl" (400, 400) (500, 500)
  in play window black 60 initialWorld draw handleInput update

