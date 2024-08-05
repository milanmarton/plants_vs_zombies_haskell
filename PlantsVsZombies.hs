module PlantsVsZombies where

import Data.Maybe

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int deriving (Eq, Show)
data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int deriving (Eq, Show)
data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving (Eq, Show)

-- 2/3 bonusz

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2

---------------------------------------------------------------------------------------------------------------------

tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel  -- Plant purchase
tryPurchase (GameModel psun plants zombies) (a, b) plant
    | a >= 5 || a < 0  || b >= 12 || b < 0 = Nothing
    | lookup (a, b) plants /= Nothing = Nothing
    | plant == defaultPeashooter && psun >= 100 = Just (GameModel (psun - 100) ([((a, b), plant)] ++ plants) zombies)
    | ((plant == defaultSunflower) || (plant == defaultWalnut)) && psun >= 50 = Just (GameModel (psun - 50) ([((a, b), plant)] ++ plants) zombies)
    | plant == defaultCherryBomb && psun >= 150 = Just (GameModel (psun - 150) ([((a, b), plant)] ++ plants) zombies)
    | otherwise = Nothing

---------------------------------------------------------------------------------------------------------------------

placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel -- placing a zombie in lane
placeZombieInLane (GameModel psun plants zombies) zombie lane
    | lane < 0 || lane >= 5 = Nothing
    | lookup (lane, 11) zombies /= Nothing = Nothing
    | otherwise = Just (GameModel psun plants ([((lane, 11), zombie)] ++ zombies))

---------------------------------------------------------------------------------------------------------------------

isjumpvault :: Zombie -> Bool  -- checks if the zombie is vaulting type
isjumpvault (Basic hp speed) = False
isjumpvault (Conehead hp speed) = False
isjumpvault (Buckethead hp speed) = False
isjumpvault (Vaulting hp 2) = True
isjumpvault (Vaulting hp speed) = False

---------------------------------------------------------------------------------------------------------------------

plantDmg :: Plant -> Plant  -- Lowering the hp of a singular plant
plantDmg (Peashooter hp) = (Peashooter (hp - 1))
plantDmg (Sunflower hp) = (Sunflower (hp - 1))
plantDmg (Walnut hp) = (Walnut (hp - 1))
plantDmg (CherryBomb hp) = (CherryBomb (hp - 1))

plantDmgRec :: (Coordinate, Plant) -> [(Coordinate, Zombie)] -> (Coordinate, Plant)  -- lowering the hp of a plant recursively
plantDmgRec plant [] = plant
plantDmgRec (pcoord, plant) [(zcoord, zombie)]
    | (pcoord == zcoord) && (not (isjumpvault zombie)) = plantDmgRec (pcoord, (plantDmg plant)) []
    | otherwise = plantDmgRec (pcoord, plant) []
plantDmgRec (pcoord, plant) ((zcoord, zombie) : zs)
    | (pcoord == zcoord) && (not (isjumpvault zombie)) = plantDmgRec (pcoord, (plantDmg plant)) zs
    | otherwise = plantDmgRec (pcoord, plant) zs

plantDmgList :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Plant)]  -- reduce the hp of all plants in a list
plantDmgList [] zombies = []
plantDmgList plants [] = plants
plantDmgList [(pcoord, plant)] zombies
    | lookup pcoord zombies /= Nothing = (plantDmgRec (pcoord, plant) zombies) : plantDmgList [] zombies
    | otherwise = (pcoord, plant) : plantDmgList [] zombies
plantDmgList ((pcoord, plant) : ps) zombies
    | lookup pcoord zombies /= Nothing = (plantDmgRec (pcoord, plant) zombies) : plantDmgList ps zombies
    | otherwise = (pcoord, plant) : plantDmgList ps zombies

---------------------------------------------------------------------------------------------------------------------

vaultmove :: (Coordinate, Zombie) -> (Coordinate, Zombie) -- vaulting zombie movement
vaultmove ((a, b), vault)
    | b < 2 = ((a, 0), vault)
    | otherwise = ((a, (b - 2)), vault)

zombiemove :: (Coordinate, Zombie) -> (Coordinate, Zombie) -- zombie movement when they dont interact with plants
zombiemove ((a, b), zombie)
    | isjumpvault zombie = vaultmove ((a, b), zombie)
    | otherwise = ((a, (b - 1)), zombie)

vaultjump :: (Coordinate, Zombie) -> (Coordinate, Zombie) -- vaulting zombie jump if its in front of the plant
vaultjump ((a, b), (Vaulting hp 2))
    | b < 2 = ((a, 0), Vaulting hp 1)
    | otherwise = ((a, (b - 2)), Vaulting hp 1)

vaultjump2 :: (Coordinate, Zombie) -> (Coordinate, Zombie) -- vaulting zombie jump if its on the plant (same coords)
vaultjump2 ((a, b), (Vaulting hp 2))
    | b < 1 = ((a, 0), Vaulting hp 1)
    | otherwise = ((a, (b - 1)), Vaulting hp 1)

zombiemoveList :: [(Coordinate, Zombie)] -> [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -- simulate the movements of zombies in a list
zombiemoveList [] plants = []
zombiemoveList [((a, b), zombie)] plants
    | (lookup (a, (b - 1)) plants /= Nothing) && (isjumpvault zombie) = (vaultjump ((a, b), zombie)) : zombiemoveList [] plants
    | (lookup (a, b) plants /= Nothing) && (isjumpvault zombie) = (vaultjump2 ((a, b), zombie)) : zombiemoveList [] plants
    | lookup (a, b) plants /= Nothing = ((a, b), zombie) : zombiemoveList [] plants
    | otherwise = (zombiemove ((a, b), zombie)) : zombiemoveList [] plants
zombiemoveList (((a, b), zombie) : zs) plants
    | (lookup (a, (b - 1)) plants /= Nothing) && (isjumpvault zombie) = (vaultjump ((a, b), zombie)) : zombiemoveList zs plants
    | (lookup (a, b) plants /= Nothing) && (isjumpvault zombie) = (vaultjump2 ((a, b), zombie)) : zombiemoveList zs plants
    | lookup (a, b) plants /= Nothing = ((a, b), zombie) : zombiemoveList zs plants
    | otherwise = (zombiemove ((a, b), zombie)) : zombiemoveList zs plants

---------------------------------------------------------------------------------------------------------------------

zombiezero :: [(Coordinate, Zombie)] -> Bool  -- checks if a zombie has reached the end of the line
zombiezero [] = False
zombiezero [((a, b), zombie)]
    | 0 == b = True
    | otherwise = zombiezero []
zombiezero (((a, b), zombie):xs)
    | 0 == b = True
    | otherwise = zombiezero xs

performZombieActions :: GameModel -> Maybe GameModel  -- simulating the actions and consequences of zombies
performZombieActions (GameModel psun plants zombies)
    | zombiezero zombies = Nothing
    | otherwise = Just (GameModel psun (plantDmgList plants zombies) (zombiemoveList zombies plants))

---------------------------------------------------------------------------------------------------------------------

plantHp :: Plant -> Int  -- Gets the hp of a plant
plantHp (Peashooter hp) = hp
plantHp (Sunflower hp) = hp
plantHp (Walnut hp) = hp
plantHp (CherryBomb hp) = hp

cleanPlants :: [(Coordinate, Plant)] -> [(Coordinate, Plant)] -- Deletes the plants with 0 (or less) hp from a list
cleanPlants [] = []
cleanPlants [(coords, plant)]
    | plantHp plant <= 0 = []
    | otherwise = [(coords, plant)]
cleanPlants ((coords, plant) : ps)
    | plantHp plant <= 0 = cleanPlants ps
    | otherwise = (coords, plant) : cleanPlants ps

zombieHp :: Zombie -> Int -- Gets the hp of a zombie
zombieHp (Basic hp speed) = hp
zombieHp (Conehead hp speed) = hp
zombieHp (Buckethead hp speed) = hp
zombieHp (Vaulting hp speed) = hp

cleanZombies :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]  -- Deletes the zombies with 0 (or less) hp from a list
cleanZombies [] = []
cleanZombies [(coords, zombie)]
    | zombieHp zombie <= 0 = []
    | otherwise = [(coords, zombie)]
cleanZombies ((coords, zombie) : ps)
    | zombieHp zombie <= 0 = cleanZombies ps
    | otherwise = (coords, zombie) : cleanZombies ps

cleanBoard :: GameModel -> GameModel  -- Deletes the entities with zero or lower hp levels from the GameModel
cleanBoard (GameModel psun plants zombies) = GameModel psun (cleanPlants plants) (cleanZombies zombies)

---------------------------------------------------------------------------------------------------------------------

isPea :: Plant -> Bool
isPea (Peashooter hp) = True
isPea _ = False

isSun :: Plant -> Bool
isSun (Sunflower hp) = True
isSun _ = False

isCherry :: Plant -> Bool
isCherry (CherryBomb hp) = True
isCherry _ = False

peashooterList :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
peashooterList [] = []
peashooterList [(coord, p)]
    | isPea p = (coord, p) : []
    | otherwise = []
peashooterList ((coord, p):ps)
    | isPea p = (coord, p) : peashooterList ps
    | otherwise = peashooterList ps

sunList :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
sunList [(coord, p)]
    | isSun p = (coord, p) : []
    | otherwise = []
sunList ((coord, p):ps)
    | isSun p = (coord, p) : sunList ps
    | otherwise = sunList ps

cherryList :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
cherryList [(coord, p)]
    | isCherry p = (coord, p) : []
    | otherwise = []
cherryList ((coord, p):ps)
    | isCherry p = (coord, p) : cherryList ps
    | otherwise = cherryList ps

sunAction :: Sun -> [(Coordinate, Plant)] -> Sun
sunAction psun [] = psun
sunAction psun plants = psun + (length ((sunList plants)) * 25)

firstZombie :: Coordinate -> [(Coordinate, Zombie)] -> Coordinate
firstZombie (a, 12) zombies = (a, 12)
firstZombie (a, b) zombies
    | lookup (a, b) zombies /= Nothing = (a, b)
    | otherwise = firstZombie (a, (b + 1)) zombies

zombieDmg :: Zombie -> Zombie
zombieDmg (Basic hp speed) = (Basic (hp - 1) speed)
zombieDmg (Conehead hp speed) = (Conehead (hp - 1) speed)
zombieDmg (Buckethead hp speed) = (Buckethead (hp - 1) speed)
zombieDmg (Vaulting hp speed) = (Vaulting (hp - 1) speed)

zombieDmgList :: [(Coordinate, Zombie)] -> Coordinate -> [(Coordinate, Zombie)]
zombieDmgList [] coord = []
zombieDmgList [((a, b), zombie)] coord
    | (a, b) == coord = [((a,b), (zombieDmg zombie))]
    | otherwise = [((a, b), zombie)]
zombieDmgList (((a, b), zombie) : zs) coord
    | (a, b) == coord = ((a,b), (zombieDmg zombie)) : zombieDmgList zs coord
    | otherwise = ((a, b), zombie) : zombieDmgList zs coord

peaAction :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
peaAction plants [] = []
peaAction [] zombies = zombies
peaAction ((pcoord, plant) : ps) zombies = peaAction ps (zombieDmgList zombies (firstZombie pcoord zombies))

coordhelper :: Coordinate -> [Coordinate]
coordhelper (a, b) = [(a, (b-1)), (a, b), (a, (b+1)), ((a-1), (b-1)), ((a-1), b), ((a-1), (b+1)), ((a+1), (b-1)), ((a+1), b), ((a+1), (b+1))]

cherryCoords :: [(Coordinate, Plant)] -> [Coordinate]
cherryCoords [] = []
cherryCoords [(pcoord, cherry)] = coordhelper pcoord
cherryCoords ((pcoord, cherry) : ps) = (coordhelper pcoord) ++ cherryCoords ps

killZombie :: Zombie -> Zombie
killZombie (Basic hp speed) = (Basic 0 speed)
killZombie (Conehead hp speed) = (Conehead 0 speed)
killZombie (Buckethead hp speed) = (Buckethead 0 speed)
killZombie (Vaulting hp speed) = (Vaulting 0 speed)

killZombieList :: [(Coordinate, Zombie)] -> [Coordinate] -> [(Coordinate, Zombie)]
killZombieList [] coords = []
killZombieList zombies [] = zombies
killZombieList [(zcoord, zombie)] coords
    | zcoord `elem` coords = [(zcoord, (killZombie zombie))]
    | otherwise = [(zcoord, zombie)]
killZombieList ((zcoord, zombie) : zs) coords
    | zcoord `elem` coords = (zcoord, (killZombie zombie)) : killZombieList zs coords
    | otherwise = (zcoord, zombie) : killZombieList zs coords

cherryKill :: Plant -> Plant
cherryKill (CherryBomb hp) = (CherryBomb 0)
cherryKill plant = plant

cherryKillList :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
cherryKillList [] = []
cherryKillList [(pcoord, plant)] = [(pcoord, cherryKill plant)]
cherryKillList ((pcoord, plant) : ps) = (pcoord, (cherryKill plant)) : cherryKillList ps

cherryAction :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
cherryAction plants [] = []
cherryAction [] zombies = zombies
cherryAction plants zombies = killZombieList zombies (cherryCoords (cherryList plants))

performPlantActions :: GameModel -> GameModel
performPlantActions (GameModel psun plants zombies) = GameModel (sunAction psun plants) (cherryKillList plants)  (cherryAction plants (peaAction (peashooterList plants) zombies))

---------------------------------------------------------------------------------------------------------------------

performPlantAttack :: GameModel -> GameModel
performPlantAttack (GameModel psun plants zombies) = GameModel psun (cherryKillList plants)  (cherryAction plants (peaAction (peashooterList plants) zombies))

performPlantSun :: GameModel -> GameModel
performPlantSun (GameModel psun plants zombies) = GameModel (sunAction psun plants) plants zombies

afterZombieAttack :: GameModel -> Maybe GameModel
afterZombieAttack (GameModel psun plants zombies) = performZombieActions (cleanBoard (performPlantAttack (GameModel psun plants zombies)))

placeZombieInLane2 :: [(Coordinate, Zombie)] -> Zombie -> Int -> [(Coordinate, Zombie)]
placeZombieInLane2 zombies zombie lane
    | lane < 0 || lane >= 5 = zombies
    | lookup (lane, 11) zombies /= Nothing = zombies
    | otherwise = [((lane, 11), zombie)] ++ zombies

placeZombieInLane3 :: [(Coordinate, Zombie)] -> [(Int, Zombie)] -> [(Coordinate, Zombie)]
placeZombieInLane3 zombies [] = zombies
placeZombieInLane3 zombies [(n,z)] = placeZombieInLane2 zombies z n
placeZombieInLane3 zombies ((n,z) : zs) = (placeZombieInLane2 zombies z n) ++ placeZombieInLane3 (placeZombieInLane2 zombies z n) zs

placeZombieInLane4 :: [(Coordinate, Zombie)] -> [[(Int, Zombie)]] -> [(Coordinate, Zombie)]
placeZombieInLane4 zombies [] = zombies
placeZombieInLane4 zombies [[(n,z)]] = placeZombieInLane3 zombies [(n,z)]
placeZombieInLane4 zombies (x:xs) = (placeZombieInLane3 zombies x) ++ placeZombieInLane4 (placeZombieInLane3 zombies x) xs

addingNewZombies :: GameModel -> [[(Int, Zombie)]] -> GameModel
addingNewZombies (GameModel psun plants zombies) newzombies = GameModel psun plants (placeZombieInLane4 zombies newzombies)

totalGame :: GameModel -> [[(Int, Zombie)]] -> GameModel
totalGame (GameModel psun plants zombies) newzombies = performPlantSun (cleanBoard (addingNewZombies ( fromJust (afterZombieAttack (GameModel psun plants zombies))) newzombies))

defendsAgainst :: GameModel -> [[(Int, Zombie)]] -> Bool
defendsAgainst (GameModel psun plants []) [] = True
defendsAgainst (GameModel psun plants zombies) newzombies
    | zombiezero zombies = False
    | otherwise = defendsAgainst (totalGame (GameModel psun plants zombies) newzombies) []

---------------------------------------------------------------------------------------------------------------------
