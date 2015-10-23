-- (C) bikramkawan@gmail.com--

import System.IO
import Control.Monad
import Data.Ord
import Data.List

-- Position in grid for drones and zones intermediate variables
type Position = (Int, Int) -- (x, y)

-- Data type for storing the game state
data GameState = GameState { playerNum     :: Int  
                           , myID          :: Int
                           , droneNum      :: Int
                           , zoneNum       :: Int 
                           , zonePositions :: [Position]
                           , zoneTeams     :: [Int]
                           , playerDrones  :: [[Position]]
                           } deriving (Show)   

-- Prettyprint a game state
showGameState :: GameState -> String
showGameState gstate = 
                "playerNum: " ++ show (playerNum gstate) ++ "\n" ++
                "myID: " ++ show (myID gstate) ++  "\n" ++
                "droneNum: " ++ show (droneNum gstate) ++ "\n" ++
                "zoneNum: " ++ show (zoneNum gstate) ++ "\n" ++
                "zonePositions: " ++ show (zonePositions gstate) ++ "\n" ++
                "zoneTeams: " ++ show (zoneTeams gstate) ++  "\n" ++
                "playerDrones: " ++ show (playerDrones gstate)

-- Convert position (x,y) to "x y"
printPosition :: Position -> String
printPosition (x, y) = show x ++ " " ++ show y
                
-- Convert list of positions to a list of strings
dronesToZones :: [Position] -> [String]
dronesToZones zPositions = map printPosition zPositions

-- Prints drone target positions 
displayPositions :: Int -> [Position] -> IO()
displayPositions d zPositions = mapM_ putStrLn posList            
            where 
                -- List of positions with length >= number of drones d
                posList = take d $ concat $ take n (repeat positionsAsStrings)
                
                -- Calculate n so posList is sufficiently long
                n = d `div` (length zPositions) + 1 :: Int
                
                -- List of position strings
                positionsAsStrings = dronesToZones zPositions

--distbwtn_dronetozone::Position->Position->Double
--distbwtn_dronetozone dpos zps = sqrt $ fromIntegral $ (fst (dpos)-fst (zps))^2+ (snd (dpos)-snd (zps))^2


--dronetozpos::Position->[Position]->[Double]
--dronetozpos drone1 zPositions = map (distbwtn_dronetozone drone1)  zPositions


--nearestPosition p ps = sortBy (comparing (distance2 p)) ps

distance2 :: Position -> Position -> Int
distance2 (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2 

distance2List :: Position -> [Position] -> [Int]
distance2List p ps = map (distance2 p) ps



p = (0,0) :: Position
ps = [(55,55), (44,44), (22,22), (33,33)] :: [Position] --Drone
ps2 = [(1,1), (11,11), (33,33), (77,77)] :: [Position]--Pos

nearestPosition :: [Position] -> Position -> [Position]
nearestPosition ps p = sortBy (comparing (distance2 p)) ps

sortAllPositions :: [Position] -> [Position] -> [[Position]]
sortAllPositions  mydronepos zPositions = dr3 4 $  map (nearestPosition zPositions) mydronepos 

--dr1 m = concatMap (take 1) . iterate (drop m)
--Take first element and every 4th element from list
--http://stackoverflow.com/questions/7599777/how-to-select-every-n-th-element-from-a-list
dr3 m = map snd . filter ((== 1) . fst) . zip (cycle [1..m])


--zone_p=sortAllPositions  mydronepos1 zPositions1::[[Position]]
--zposlist = dr3 4 $ sortAllPositions  mydronepos zPositions ::[[Position]]

--sorting::Position->[Position]->[(Double,Position)]
----sorting drone1 zPositions = map (dronetozpos drone1 zPositions) zPositions

-- Main function                        
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    -- Get some initialisation input data about the game
    input_line <- getLine
    let input = words input_line
    let p = read (input!!0)  :: Int -- number of players in the game (2 to 4)
    let id = read (input!!1) :: Int -- ID of your player (0, 1, 2, or 3)
    let d = read (input!!2)  :: Int -- number of drones in each team (3 to 11)
    let z = read (input!!3)  :: Int -- number of zones on the map (4 to 8)
    
    -- Display input data
    hPutStrLn stderr $ "Number of players: " ++ show p
    hPutStrLn stderr $ "My player ID: " ++ show id
    hPutStrLn stderr $ "Number of drones: " ++ show d
    hPutStrLn stderr $ "Number of zones: " ++ show z
        
    -- Get positions of the centres of zones, all with radius of 100 units
    zonePos <- replicateM z $ do
        input_line <- getLine
        let input = words input_line
        let x = read (input!!0) :: Int 
        let y = read (input!!1) :: Int
        return (x,y)
        
    -- Display zone positions
    hPutStrLn stderr $ "Zone positions: " ++ show zonePos
    
    -- Save all information about the game state
    let gameState = GameState { playerNum     = p
                              , myID          = id
                              , droneNum      = d
                              , zoneNum       = z
                              , zonePositions = zonePos
                              , zoneTeams     = [] 
                              , playerDrones  = [] 
                              }
    
    -- Display gameState
    hPutStrLn stderr $ show gameState
    
    -- Alternative way of displaying gameState
    hPutStrLn stderr $ showGameState gameState
    
    loop gameState

loop :: GameState -> IO ()
loop gstate = do
    -- Extract some game state information
    let z = zoneNum gstate
    let p = playerNum gstate
    let d = droneNum gstate
    let zPositions = zonePositions gstate
    let meid = myID gstate
    --putStrLn $show meid ++"bikramname"
    
    -- List of teams controlling the zones (ordered as in the initialisation)
    zTeams <- replicateM z $ do
        input_line <- getLine
        let tid = read input_line :: Int -- team ID (0,1,2,3), -1 (uncontrolled)
        return (tid)
    
    -- Display the list of teams controlling the zones
    hPutStrLn stderr $ "Team controlling each zone: " ++ show zTeams    
    
    -- Ordered list of lists of drones' positions for each player
    pDrones <- replicateM p $ do
         -- Positions of drones belonging to player p
        drones <- replicateM d $ do
            input_line <- getLine
            let input = words input_line
            let dx = read (input!!0) :: Int 
            let dy = read (input!!1) :: Int
            return (dx,dy)
        return (drones)
    
    -- Display the list of drone positions for each player
    hPutStrLn stderr $ "Drone positions for each player:" 
    hPutStrLn stderr $ show pDrones
    
    -- Update game state
    let newGameState = GameState { playerNum     = playerNum gstate
                                 , myID          = myID gstate
                                 , droneNum      = droneNum gstate
                                 , zoneNum       = zoneNum gstate
                                 , zonePositions = zonePositions gstate
                                 , zoneTeams     = zTeams :: [Int]
                                 , playerDrones  = pDrones :: [[Position]]
                                 }

    -- Display the new game state
    --hPutStrLn stderr $ showGameState newGameState  
    let mydronepos = pDrones!!meid
    --let mydronepos1=mydronepos
    hPutStrLn stderr $show mydronepos ++"My Drone Position"




    --let mydroneposition = mydronepos!!meid
    --putStrLn $show mydroneposition++"bikram drone"

    --hPutStrLn stderr $show dronetozpos drone1 zPositions

    hPutStrLn stderr $show (zPositions!!0)++"zPositions!!0"
    hPutStrLn stderr $show (mydronepos!!0)++"mydronepos!!0"

    let distance = abs (fst (zPositions!!0)-fst (mydronepos!!0))+abs (snd (zPositions!!0)-snd (mydronepos!!0))
    hPutStrLn stderr $show distance++"hei"
    -- Target coordinates for each of your drones
    -- This is where you have to do some fancy AI!
    
    -- Naiive solution: Move drone 1 to zone 1, drone 2 to zone 2, etc.
    -- Repeat if number of drones is bigger than number of zones for remaining
    -- drones
    --let dronsaal = sortAllPositions mydronepos zPositions
    --putStrLn $show dronsaal ++"dronsaal"

    
    let zposall = sortAllPositions mydronepos zPositions
    hPutStrLn stderr $show zposall ++"zposall"

    
    
    --let disp= displayPositions d $ concat zposall
    --putStrLn $show disp
    
    displayPositions d $ concat zposall

   -- displayPositions d zPositions
    
--    replicateM d $ do
--        putStrLn "20 20"
--        return ()
    
    loop newGameState