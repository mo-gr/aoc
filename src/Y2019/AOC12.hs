{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Y2019.AOC12
    ( solution1
    , solution2
    )
where

import           Control.Monad (liftM2)
import qualified Hedgehog      as H

type Value = Int
data Vec3 = Vec3 {_x :: Value, _y :: Value, _z :: Value} deriving (Show, Eq, Ord)

newtype Velocity = Velocity {unVelocity :: Vec3} deriving (Show, Eq, Ord)
newtype Position = Position {unPosition :: Vec3} deriving (Show, Eq, Ord)

data Moon = Moon {name::String, position::Position, velocity::Velocity} deriving (Show, Eq)

makeMoon :: String -> Vec3 -> Moon
makeMoon name' p = Moon name' (Position p) (Velocity $ Vec3 0 0 0)

io :: Moon
io = makeMoon "io" $ Vec3 9 13 (-8)
europa :: Moon
europa = makeMoon "europa" $ Vec3 (-3) 16 (-17)
ganymede :: Moon
ganymede = makeMoon "ganymede" $ Vec3 (-4) 11 (-10)
callisto :: Moon
callisto = makeMoon "callisto" $ Vec3 0 (-2) (-2)

universe :: [Moon]
universe = [io, europa, ganymede, callisto]

_sample_universe :: [Moon]
_sample_universe =
    [ makeMoon "m1" (Vec3 (-1) 0 2)
    , makeMoon "m2" (Vec3 2 (-10) (-7))
    , makeMoon "m3" (Vec3 4 (-8) 8)
    , makeMoon "m4" (Vec3 3 5 (-1))
    ]


kineticEnergy :: Moon -> Value
kineticEnergy Moon { velocity = Velocity Vec3 { _x, _y, _z } } =
    abs _x + abs _y + abs _z

potentialEnergy :: Moon -> Value
potentialEnergy Moon { position = Position Vec3 { _x, _y, _z } } =
    abs _x + abs _y + abs _z

totalEnergy :: Moon -> Value
totalEnergy = liftM2 (*) kineticEnergy potentialEnergy

totalEnergySystem :: [Moon] -> Value
totalEnergySystem = sum . fmap totalEnergy

gravityPull :: Moon -> Moon -> Moon
gravityPull m@Moon { position = Position pVec, velocity } Moon { position = Position pVec' }
    = m { velocity = velPlus velocity gravity }
    where gravity = Velocity (delta pVec pVec')

velPlus :: Velocity -> Velocity -> Velocity
velPlus (Velocity vVec) (Velocity vVec') = Velocity (vVec `plus` vVec')

plus :: Vec3 -> Vec3 -> Vec3
plus Vec3 { _x, _y, _z } Vec3 { _x = _x', _y = _y', _z = _z' } =
    Vec3 (_x + _x') (_y + _y') (_z + _z')

delta :: Vec3 -> Vec3 -> Vec3
delta Vec3 { _x, _y, _z } Vec3 { _x = _x', _y = _y', _z = _z' } = Vec3
    (f _x _x')
    (f _y _y')
    (f _z _z')
  where
    f x x' | x < x' = 1
    f x x' | x > x' = -1
    f _ _  = 0

applyVelocity :: Moon -> Moon
applyVelocity m@Moon { velocity = Velocity vVec, position = Position pVec } =
    m { position = Position (vVec `plus` pVec) }

applyGravity :: [Moon] -> Moon -> Moon
applyGravity []           m = m
applyGravity (other : ms) m = applyGravity ms (m `gravityPull` other)

tick :: [Moon] -> [Moon]
tick ms = applyVelocity . applyGravity ms <$> ms

times :: Int -> (a -> a) -> a -> a
times 0 _ a = a
times x f a = times (x - 1) f (f a)

countTimesTil :: Int -> (a -> Bool) -> (a -> a) -> a -> (a, Int)
countTimesTil n pred' _ a | pred' a = (a, n)
countTimesTil n pred' f a = countTimesTil (n + 1) pred' f (f a)

-- 7758
solution1 :: IO Int
solution1 = return . totalEnergySystem $ times 1000 tick universe

data Dimension = X | Y | Z

oneDimensionEq :: Dimension -> Moon -> Moon -> Bool
oneDimensionEq X Moon { velocity = Velocity Vec3 { _x = vx }, position = Position Vec3 { _x = px } } Moon { velocity = Velocity Vec3 { _x = vx' }, position = Position Vec3 { _x = px' } }
    = px == px' && vx == vx'
oneDimensionEq Y Moon { velocity = Velocity Vec3 { _y = vy }, position = Position Vec3 { _y = py } } Moon { velocity = Velocity Vec3 { _y = vy' }, position = Position Vec3 { _y = py' } }
    = py == py' && vy == vy'
oneDimensionEq Z Moon { velocity = Velocity Vec3 { _z = vz }, position = Position Vec3 { _z = pz } } Moon { velocity = Velocity Vec3 { _z = vz' }, position = Position Vec3 { _z = pz' } }
    = pz == pz' && vz == vz'

lcm' :: Integral a => [a] -> a
lcm' []       = 1
lcm' (a : as) = lcm a (lcm' as)

-- 354540398381256
solution2 :: IO ()
solution2 = do
    let universe' = tick universe
    let perMoon d = snd $ countTimesTil
            1
            (\uni -> and $ zipWith (oneDimensionEq d) uni universe)
            tick
            universe'
    print . lcm' $ perMoon <$> [X, Y, Z]


prop_step_0 :: H.Property
prop_step_0 = H.withTests 1 $ H.property $ do
    let step0 = times 0 tick _sample_universe
    step0 H.=== _sample_universe

prop_step_1 :: H.Property
prop_step_1 = H.withTests 1 $ H.property $ do
    let step0 = times 1 tick _sample_universe
    step0
        H.=== [ Moon { name     = "m1"
                     , position = Position (Vec3 { _x = 2, _y = -1, _z = 1 })
                     , velocity = Velocity (Vec3 { _x = 3, _y = -1, _z = -1 })
                     }
              , Moon { name     = "m2"
                     , position = Position (Vec3 { _x = 3, _y = -7, _z = -4 })
                     , velocity = Velocity (Vec3 { _x = 1, _y = 3, _z = 3 })
                     }
              , Moon { name     = "m3"
                     , position = Position (Vec3 { _x = 1, _y = -7, _z = 5 })
                     , velocity = Velocity (Vec3 { _x = -3, _y = 1, _z = -3 })
                     }
              , Moon { name     = "m4"
                     , position = Position (Vec3 { _x = 2, _y = 2, _z = 0 })
                     , velocity = Velocity (Vec3 { _x = -1, _y = -3, _z = 1 })
                     }
              ]

prop_velocity :: H.Property
prop_velocity = H.withTests 1 $ H.property $ do
    let m  = head _sample_universe
    let m' = applyGravity _sample_universe m
    velocity m' H.=== Velocity (Vec3 { _x = 3, _y = -1, _z = -1 })

prop_velocity_simple :: H.Property
prop_velocity_simple = H.withTests 1 $ H.property $ do
    let m  = head _sample_universe
    let m' = applyGravity [m] m
    velocity m' H.=== Velocity (Vec3 { _x = 0, _y = 0, _z = 0 })

prop_velocity_one :: H.Property
prop_velocity_one = H.withTests 1 $ H.property $ do
    let m  = head _sample_universe
    let m' = applyGravity (take 2 _sample_universe) m
    velocity m' H.=== Velocity (Vec3 { _x = 1, _y = -1, _z = -1 })

prop_gravity_neutral :: H.Property
prop_gravity_neutral = H.withTests 1 $ H.property $ do
    let m = head _sample_universe
    velocity (m `gravityPull` m)
        H.=== Velocity (Vec3 { _x = 0, _y = 0, _z = 0 })

prop_gravity_pull :: H.Property
prop_gravity_pull = H.withTests 1 $ H.property $ do
    let m  = head _sample_universe
    let m' = head (tail _sample_universe)
    velocity (m `gravityPull` m')
        H.=== Velocity (Vec3 { _x = 1, _y = -1, _z = -1 })

_tests :: IO Bool
_tests = H.checkParallel $ H.Group
    "AOC12"
    [ ("prop_step_0"         , prop_step_0)
    , ("prop_step_1"         , prop_step_1)
    , ("prop_velocity_simple", prop_velocity_simple)
    , ("prop_velocity_one"   , prop_velocity_one)
    , ("prop_velocity"       , prop_velocity)
    , ("prop_gravity_neutral", prop_gravity_neutral)
    , ("prop_gravity_pull"   , prop_gravity_pull)
    ]
