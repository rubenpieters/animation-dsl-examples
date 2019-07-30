{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Example3 where

import Prelude hiding (seq)

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find, findIndex, repeat)

import System.Random

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Graphics.Gloss hiding (scale, color)
import Graphics.Gloss.Interface.Pure.Game hiding (scale, color)

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
  Get :: Lens' obj a -> Ops obj a

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

dslGet :: Lens' obj a -> Dsl (Ops obj) a
dslGet lens = Bind (Get lens) (\a -> Return a)

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
applyOp obj t (Get lens) = let
  value = obj ^. lens
  in (obj, Right value)

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

hexagonPic :: Picture
hexagonPic = lineLoop [(2, 3.5), (4, 0), (2, -3.5), (-2, -3.5), (-4, 0), (-2, 3.5)]

circlePic :: Picture
circlePic = ThickCircle 0.3 15

particlePic :: Picture
particlePic = Circle 5

bulletPic :: Picture
bulletPic = circleSolid 1.5

bombPic :: Picture
bombPic = Circle 1

barPic :: Float -> Picture
barPic w = let
  w' = w / 50
  in Polygon [(0, 0), (w', 0), (w', 1), (0, 1), (0, 0)]

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
  , _playerIndicators :: [Sprite]
  , _playerHp :: Float
  , _shootCD :: Float
  , _bombCD :: Float
  , _hitCD :: Float
  }

makeLenses ''Player

data Enemy
  = Enemy
  { _enemySprite :: Sprite
  , _enemyPartTriangle1 :: Sprite
  , _enemyPartTriangle2 :: Sprite
  , _enemyPartCannon :: [Sprite]
  , _enemyHpBar :: Sprite
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
  , _playerAnims :: [Dsl (Ops World) ()]
  , _bullets :: [Bullet]
  , _enemy :: Enemy
  , _enemyAnims :: [Dsl (Ops World) ()]
  , _enemyBullets :: [Bullet]
  , _keysDown :: Set Key
  , _mouse :: (Float, Float)
  , _particles :: [Sprite]
  , _nextParticleId :: Int
  , _particleAnims :: [Dsl (Ops World) ()]
  , _introVisible :: Bool
  , _gameVisible :: Bool
  }

makeLenses ''World

gameSprites :: World -> [Sprite]
gameSprites w =
  map _bulletSprite (w ^. bullets) ++
  map _bulletSprite (w ^. enemyBullets) ++
  [w ^. enemy . enemySprite] ++
  w ^. enemy . enemyPartCannon ++
  [w ^. player . playerSprite] ++
  w ^. player . playerIndicators ++
  w ^. particles ++
  [w ^. enemy . enemyHpBar]

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

createBomb :: (Float, Float) -> World -> (World, Int)
createBomb (x, y) w@(World {_particles, _nextParticleId}) = let
  newIndex = w ^. nextParticleId
  particle = Sprite x y 1 (0.7, 0.35, 0.5) 1 0 particlePic newIndex
  newWorld = w { _particles = particle : _particles, _nextParticleId = _nextParticleId + 1 }
  in (newWorld, newIndex)

deleteBomb :: Int -> World -> World
deleteBomb id w@(World {_particles}) = let
  newWorld = w { _particles = filter (\x -> x ^. spriteId /= id) _particles }
  in newWorld

createParticle :: (Float, Float) -> World -> (World, Int)
createParticle (x, y) w@(World {_particles, _nextParticleId}) = let
  newIndex = w ^. nextParticleId
  particle = Sprite x y 1 (1, 1, 1) 3 0 bombPic newIndex
  newWorld = w { _particles = particle : _particles, _nextParticleId = _nextParticleId + 1 }
  in (newWorld, newIndex)

deleteParticle :: Int -> World -> World
deleteParticle id w@(World {_particles}) = let
  newWorld = w { _particles = filter (\x -> x ^. spriteId /= id) _particles }
  in newWorld

createHpBarParticle :: (Float, Float) -> World -> (World, Int)
createHpBarParticle (x, y) w@(World {_particles, _nextParticleId}) = let
  newIndex = w ^. nextParticleId
  particle = Sprite x y 1 (1, 0, 0) 1 0 (circleSolid 4) newIndex
  newWorld = w { _particles = particle : _particles, _nextParticleId = _nextParticleId + 1 }
  in (newWorld, newIndex)

createEnBullet1 :: (Float, Float) -> (Float, Float) -> World -> (World, Int)
createEnBullet1 (x, y) dir w@(World {_enemyBullets}) = let
  bullet = Bullet
    (Sprite x y 1 (0.8, 0.1, 0.75) 3 0 bulletPic undefined)
    dir
  newWorld = w { _enemyBullets = bullet : _enemyBullets }
  in (newWorld, undefined)

enemyHitParticleAnim :: (Float, Float) -> Dsl (Ops World) ()
enemyHitParticleAnim pos@(paX, paY) = let
  paDur = 0.5
  particleF (offX, offY) = do
      i <- create (createParticle pos)
      par
        [ basic (For paDur) (particles . paId i . x) (To (paX + offX * 10))
        , basic (For paDur) (particles . paId i . y) (To (paY + offY * 10))
        , basic (For paDur) (particles . paId i . alpha) (To 0.3)
        , basic (For paDur) (particles . paId i . scale) (To 0.5)
        ]
      delete deleteParticle i
  in par (map particleF [(1, 1), (-1, -1), (1, -1), (-1, 1)])

enemyHitAnim :: Dsl (Ops World) ()
enemyHitAnim = seq
  [ basic (For 0.05) (enemy . enemySprite . alpha) (To 0.3)
  , basic (For 0.02) (enemy . enemySprite . alpha) (To 1)
  ]

enemyHpBarParticle :: Dsl (Ops World) ()
enemyHpBarParticle = let
  createParticle offY = do
    hp <- dslGet (enemy . enemyHp)
    let xLoc = (-100) + (hp / 5)
    i <- create (createHpBarParticle (xLoc, 156 + offY))
    par
      [ basic (For 0.3) (particles . paId i . x) (To (xLoc + 15))
      , basic (For 0.3) (particles . paId i . alpha) (To 0.3)
      , basic (For 0.3) (particles . paId i . scale) (To 0.5)
      ]
    delete deleteParticle i
    in seq (map createParticle [1, 7, 4])

playerHitAnim :: Dsl (Ops World) ()
playerHitAnim = seq
  [ basic (For 0.5) (player . playerSprite . alpha) (To 0.15)
  , basic (For 0.5) (player . playerSprite . alpha) (To 1)
  , basic (For 0.5) (player . playerSprite . alpha) (To 0.15)
  , basic (For 0.5) (player . playerSprite . alpha) (To 1)
  , basic (For 0.25) (player . playerSprite . alpha) (To 0.15)
  , basic (For 0.25) (player . playerSprite . alpha) (To 1)
  , basic (For 0.25) (player . playerSprite . alpha) (To 0.15)
  , basic (For 0.25) (player . playerSprite . alpha) (To 1)
  ]

playerPulse :: Dsl (Ops World) ()
playerPulse = do
  hp <- dslGet (player . playerHp)
  seq
    [ if hp > 0
        then basic (For 0.4) (player . playerIndicators . atIndex 0 . scale) (To 15)
        -- else noop
        else basic (For 0.4) (player . playerIndicators . atIndex 0 . scale) (To 0)
    , if hp > 1
        then basic (For 0.4) (player . playerIndicators . atIndex 1 . scale) (To 15)
        -- else noop
        else basic (For 0.4) (player . playerIndicators . atIndex 1 . scale) (To 0)
    , if hp > 2
        then basic (For 0.4) (player . playerIndicators . atIndex 2 . scale) (To 15)
        -- else noop
        else basic (For 0.4) (player . playerIndicators . atIndex 2 . scale) (To 0)
    , par
        [ basic (For 0.2) (player . playerIndicators . atIndex 0 . alpha) (To 0)
        , basic (For 0.2) (player . playerIndicators . atIndex 1 . alpha) (To 0)
        , basic (For 0.2) (player . playerIndicators . atIndex 2 . alpha) (To 0)
        ]
    , par
        [ basic (For 0) (player . playerIndicators . atIndex 0 . scale) (To 0)
        , basic (For 0) (player . playerIndicators . atIndex 1 . scale) (To 0)
        , basic (For 0) (player . playerIndicators . atIndex 2 . scale) (To 0)
        , basic (For 0) (player . playerIndicators . atIndex 0 . alpha) (To 1)
        , basic (For 0) (player . playerIndicators . atIndex 1 . alpha) (To 1)
        , basic (For 0) (player . playerIndicators . atIndex 2 . alpha) (To 1)
        ]
    ]
  playerPulse

playerBombAnim :: (Float, Float) -> Dsl (Ops World) ()
playerBombAnim pos = do
  i <- create (createBomb pos)
  par
    [ basic (For 0.05) (particles . paId i . scale) (To 10)
    , basic (For 0.05) (particles . paId i . alpha) (To 0.5)
    ]
  basic (For 0.2) (particles . paId i . alpha) (To 0)

ints :: [Int]
ints = randoms (mkStdGen 179424691)
  & map (\x -> x `mod` 6)

enemyAnim :: Dsl (Ops World) ()
enemyAnim = let
  movement = seq
    [ par
        [ basic (For 4) (enemy . enemySprite . x) (To (150))
        , basic (For 4) (enemy . enemyPartTriangle1 . x) (To (150))
        , basic (For 4) (enemy . enemyPartTriangle2 . x) (To (150))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 0 . x) (To (150))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 1 . x) (To (189))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 2 . x) (To (189))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 3 . x) (To (150))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 4 . x) (To (111))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 5 . x) (To (111))
        ]
    , par
        [ basic (For 4) (enemy . enemySprite . x) (To (-150))
        , basic (For 4) (enemy . enemyPartTriangle1 . x) (To (-150))
        , basic (For 4) (enemy . enemyPartTriangle2 . x) (To (-150))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 0 . x) (To (-150))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 1 . x) (To (-111))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 2 . x) (To (-111))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 3 . x) (To (-150))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 4 . x) (To (-189))
        , basic (For 4) (enemy . enemyPartCannon . atIndex 5 . x) (To (-189))
        ]
    , movement
    ]
  spawnBullet (offX, offY) dir = do
    x <- dslGet (enemy . enemySprite . x)
    y <- dslGet (enemy . enemySprite . y)
    _ <- create (createEnBullet1 (x + offX, y + offY) dir)
    return ()
  windup :: Lens' Enemy Sprite -> Dsl (Ops World) ()
  windup cannon = par
    [ seq
        [ basic (For 0.5) (enemy . cannon . rotation) (To 360)
        , basic (For 0) (enemy . cannon . rotation) (To 0)
        , basic (For 0.5) (enemy . cannon . rotation) (To 360)
        , basic (For 0) (enemy . cannon . rotation) (To 0)
        , basic (For 0.5) (enemy . cannon . rotation) (To 360)
        , basic (For 0) (enemy . cannon . rotation) (To 0)
        , basic (For 0.25) (enemy . cannon . rotation) (To 360)
        , basic (For 0) (enemy . cannon . rotation) (To 0)
        , basic (For 0.25) (enemy . cannon . rotation) (To 360)
        , basic (For 0) (enemy . cannon . rotation) (To 0)
        , basic (For 0.25) (enemy . cannon . rotation) (To 360)
        , basic (For 0) (enemy . cannon . rotation) (To 0)
        , basic (For 0.125) (enemy . cannon . rotation) (To 360)
        , basic (For 0) (enemy . cannon . rotation) (To 0)
        , basic (For 0.125) (enemy . cannon . rotation) (To 360)
        , basic (For 0) (enemy . cannon . rotation) (To 0)
        ]
    , seq
        [ par
          [ basic (For 2.5) (enemy . cannon . color . _2) (To 0.5)
          , basic (For 2.5) (enemy . cannon . color . _3) (To 0.5)
          ]
        , par
           [ basic (For 0) (enemy . cannon . color . _2) (To 1)
           , basic (For 0) (enemy . cannon . color . _3) (To 1)
           ]
        ]
    ]
  bulletPattern offset = [1..60] &
    map (\x -> (cos (x / 10) / 5, sin (x / 10) / 5)) &
    map (spawnBullet offset)
  offset 0 = (0, 45)
  offset 1 = (39, 22)
  offset 2 = (39, -22)
  offset 3 = (0, -45)
  offset 4 = (-39, -22)
  offset 5 = (-39, 22)
  sequence l = let
    f i = seq $ windup (enemyPartCannon . atIndex i) : bulletPattern (offset i)
    in do
         enHp <- dslGet (enemy . enemyHp)
         let amt = ceiling (6 - (enHp * 6 / 1000))
             nrs = take amt l
         par (nrs & map f)
         sequence (drop amt l)
  in par
    [ seq
      [ par
          [ basic (For 2) (enemy . enemySprite . x) (To (-150))
          , basic (For 2) (enemy . enemyPartTriangle1 . x) (To (-150))
          , basic (For 2) (enemy . enemyPartTriangle2 . x) (To (-150))
          , basic (For 2) (enemy . enemyPartCannon . atIndex 0 . x) (To (-150))
          , basic (For 2) (enemy . enemyPartCannon . atIndex 1 . x) (To (-111))
          , basic (For 2) (enemy . enemyPartCannon . atIndex 2 . x) (To (-111))
          , basic (For 2) (enemy . enemyPartCannon . atIndex 3 . x) (To (-150))
          , basic (For 2) (enemy . enemyPartCannon . atIndex 4 . x) (To (-189))
          , basic (For 2) (enemy . enemyPartCannon . atIndex 5 . x) (To (-189))
          ]
      , movement
      ]
    , sequence ints
    ]

-- Initial Definitions

initialPlayer :: Player
initialPlayer = Player
  (Sprite 0 0 1 (1, 1, 1) 10 0 trianglePic undefined)
  [ Sprite 0 0 1 (0, 0.8, 0.8) 0 0 (Circle 0.5) undefined
  , Sprite 0 0 1 (0, 0.8, 0.8) 0 0 (Circle 0.5) undefined
  , Sprite 0 0 1 (0, 0.8, 0.8) 0 0 (Circle 0.5) undefined
  ]
  3
  0
  0
  0

mkEnemyHpBar :: Float -> Sprite
mkEnemyHpBar w =
  Sprite (-100) 155 1 (1, 0.1, 0.12) 10 0 (barPic w) undefined

initialEnemy :: Enemy
initialEnemy = Enemy
  (Sprite 0 100 1 (1, 1, 1) 45 0 (Circle 1) undefined)
  (Sprite 0 90 1 (1, 1, 1) 40 180 trianglePic undefined)
  (Sprite 0 110 1 (1, 1, 1) 40 0 trianglePic undefined)
  [ Sprite 0 145 1 (1, 1, 1) 3 0 hexagonPic undefined
  , Sprite 39 122 1 (1, 1, 1) 3 0 hexagonPic undefined
  , Sprite 39 78 1 (1, 1, 1) 3 0 hexagonPic undefined
  , Sprite 0 55 1 (1, 1, 1) 3 0 hexagonPic undefined
  , Sprite (-39) 78 1 (1, 1, 1) 3 0 hexagonPic undefined
  , Sprite (-39) 122 1 (1, 1, 1) 3 0 hexagonPic undefined
  ]
  (mkEnemyHpBar 1000)
  1000

initialWorld :: World
initialWorld = World initialPlayer [playerPulse] [] initialEnemy [enemyAnim] [] Set.empty (0, 0) [] 1 []

-- Gloss Functions

overlapEn :: Bullet -> Enemy -> Bool
overlapEn bullet enemy = let
  eX = enemy ^. enemySprite . x
  eY = enemy ^. enemySprite . y
  bX = bullet ^. bulletSprite . x
  bY = bullet ^. bulletSprite . y
  dist = sqrt ((bX - eX) * (bX - eX) + (bY - eY) * (bY - eY))
  in dist <= 45

overlapP :: Bullet -> Player -> Bool
overlapP bullet player = let
  eX = player ^. playerSprite . x
  eY = player ^. playerSprite . y
  bX = bullet ^. bulletSprite . x
  bY = bullet ^. bulletSprite . y
  dist = sqrt ((bX - eX) * (bX - eX) + (bY - eY) * (bY - eY))
  in dist <= 5

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
  gameVis = w ^. gameVisible
  introVis = w ^. introVisible
  in case (introVis, gameVis) of
    (True, _) -> Pictures []
    (_, True) -> Pictures (map drawSprite (w ^. gameSprites))

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
  if Set.member (Char 'w') keys || Set.member (Char 'z') keys
    then do
      player . playerSprite . y %= \x -> x + playerMoveSpeed
      player . playerIndicators . traverse . y %= \x -> x + playerMoveSpeed
    else return ()
  if Set.member (Char 'a') keys || Set.member (Char 'q') keys
    then do
      player . playerSprite . x %= \x -> x - playerMoveSpeed
      player . playerIndicators . traverse . x %= \x -> x - playerMoveSpeed
    else return ()
  if Set.member (Char 's') keys
    then do
      player . playerSprite . y %= \x -> x - playerMoveSpeed
      player . playerIndicators . traverse . y %= \x -> x - playerMoveSpeed
    else return ()
  if Set.member (Char 'd') keys
    then do
      player . playerSprite . x %= \x -> x + playerMoveSpeed
      player . playerIndicators . traverse . x %= \x -> x + playerMoveSpeed
    else return ()
  -- player shooting
  player . shootCD %= \x -> x - 1
  sCD <- use $ player . shootCD
  if Set.member (SpecialKey KeySpace) keys && sCD <= 0
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
      player . shootCD .= 10
    else return ()
  -- player bomb
  player . bombCD %= \x -> x - 1
  bCD <- use $ player . bombCD
  if Set.member (Char 'e') keys && bCD <= 0
    then do
      playerX <- use $ player . playerSprite . x
      playerY <- use $ player . playerSprite . y
      particleAnims %= \x -> (playerBombAnim (playerX, playerY)) : x
      player . bombCD .= 5000
      -- clear bullets in radius of bomb
      w <- get
      let (newOps, newBullets) = clearBullets (playerX, playerY) 80 (w ^. enemyBullets)
      put (w { _particleAnims = newOps ++ w ^. particleAnims, _enemyBullets = newBullets})
    else return ()
  -- reorient player
  modify reorient
  -- update player bullets
  w <- get
  let (newOps, newBullets) = updateBullets w (w ^. bullets)
  put (w { _particleAnims = newOps ++ w ^. particleAnims, _bullets = newBullets})
  let playerHits = length newOps
  enemy . enemyHp %= \x -> max 0 (x - fromIntegral playerHits)
  enHp <- use $ enemy . enemyHp
  enemy . enemyHpBar .= mkEnemyHpBar enHp
  if playerHits > 0
    then enemyAnims %= \x -> enemyHitAnim : x
    else return ()
  -- update enemy bullets
  w <- get
  let (newOps, newBullets) = updateEnBullets w (w ^. enemyBullets)
  put (w { _playerAnims = newOps ++ w ^. playerAnims, _enemyBullets = newBullets})
  let enemyHits = length newOps
  if enemyHits > 0
    then do
      player . playerHp %= \x -> x - 1
      -- clear bullets when player is hit
      w <- get
      playerX <- use $ player . playerSprite . x
      playerY <- use $ player . playerSprite . y
      let (newOps, newBullets) = clearBullets (playerX, playerY) 80 (w ^. enemyBullets)
      put (w { _particleAnims = playerBombAnim (playerX, playerY) : newOps ++ w ^. particleAnims, _enemyBullets = newBullets})
    else return ()
  -- update player animations
  runAnim playerAnims t
  -- update enemy animations
  runAnim enemyAnims t
  -- update particle animations
  runAnim particleAnims t

runAnim :: (MonadState World m) =>
  Lens' World [Dsl (Ops World) ()] -> Float -> m ()
runAnim lens t = do
  w <- get
  let (newWorld, newOps) = applyOps w t (w ^. lens)
      cleanedOps = cleanAnims newOps
      newWorld' = newWorld & lens .~ cleanedOps
  put newWorld'

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
              then (enemyHitParticleAnim (bX, bY) : enemyHpBarParticle : newAnims, newList)
              else (newAnims, newBullet : newList)

updateEnBullets :: World -> [Bullet] -> ([Dsl (Ops World) ()], [Bullet])
updateEnBullets w [] = ([], [])
updateEnBullets w (bullet:r) = let
  bX = bullet ^. bulletSprite . x
  bY = bullet ^. bulletSprite . y
  p = w ^. player
  newBullet = updateBullet bullet
  (newAnims, newList) = updateEnBullets w r
  in if outsideBounds newBullet
       then (newAnims, newList)
       else if overlapP newBullet p
              then (playerHitAnim : newAnims, newList)
              else (newAnims, newBullet : newList)

updateBullet :: Bullet -> Bullet
updateBullet bullet = let
  (dirX, dirY) = bullet ^. bulletDirection
  in bullet & bulletSprite . x %~ (\x -> x + bulletSpeed * dirX)
            & bulletSprite . y %~ (\x -> x + bulletSpeed * dirY)

clearBullets :: (Float, Float) -> Float -> [Bullet] -> ([Dsl (Ops World) ()], [Bullet])
clearBullets center@(cX, cY) radius [] = ([], [])
clearBullets center@(cX, cY) radius (bullet:r) = let
  bX = bullet ^. bulletSprite . x
  bY = bullet ^. bulletSprite . y
  dist = sqrt ((bX - cX) * (bX - cX) + (bY - cY) * (bY - cY))
  (newAnims, newList) = clearBullets center radius r
  in if dist <= radius
       then (enemyHitParticleAnim (bX, bY) : newAnims, newList)
       else (newAnims, bullet : newList)

main :: IO ()
main = let
  window = InWindow "animation-dsl" (400, 400) (900, 900)
  in play window black 60 initialWorld draw handleInput update

