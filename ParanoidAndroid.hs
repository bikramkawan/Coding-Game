
--Bikram Kawan < bikramkawan@gmail.com >

import System.IO
import Control.Monad
import Data.Ord
import Data.List

type Targetpos = Int
data GameState = GameState { nbfloors :: Int  
                           , width    :: Int
                           , nbrounds :: Int
                           , exitfloor:: Int 
                           , exitpos :: Int
                           , nbtotalclones ::Int
                           , nbadditionalelevators     :: Int
                           , nbelevators  :: Int
                           ,elevators ::[(Int,Int)]
                           ,target::Int
                           } deriving (Show) 
showGameState :: GameState -> String
showGameState gstate = 
                "nbfloors: " ++ show (nbfloors gstate) ++ "\n" ++
                "width: " ++ show (width gstate) ++  "\n" ++
                "nbrounds: " ++ show (nbrounds gstate) ++ "\n" ++
                "exitfloor: " ++ show (exitfloor gstate) ++ "\n" ++
                "exitpos: " ++ show (exitpos gstate) ++ "\n" ++
                "nbadditionalelevators: " ++ show (nbadditionalelevators gstate) ++  "\n" ++
                "nbelevators: " ++ show (nbelevators gstate)++
                "elevators:"++ show (elevators gstate)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = words input_line
    hPutStrLn stderr $show input++"Input"
    let n = read (input!!0) :: Int -- number of floors
    hPutStrLn stderr $show n++"nbfloors"
    let w= read (input!!1) :: Int -- width of the area
    hPutStrLn stderr $show w ++"width"
    let nb = read (input!!2) :: Int -- maximum number of rounds
    let exitf = read (input!!3) :: Int -- floor on which the exit is found
    let exitp = read (input!!4) :: Int -- position of the exit on its floor
    let nclones = read (input!!5) :: Int -- number of generated clones
    hPutStrLn stderr $show nclones++"total num of clones"
    let nbe = read (input!!6) :: Int -- ignore (always zero)
    let ne = read (input!!7) :: Int -- number of elevators
    let t =0

    elevators1<-replicateM ne $ do
        input_line <- getLine
        let input = words input_line
        let elevatorfloor = read (input!!0) :: Int -- floor on which this elevator is found
        hPutStrLn stderr $show elevatorfloor ++"elevatorfloor"
        let elevatorpos = read (input!!1) :: Int -- position of the elevator on its floor
        --hPutStrLn stderr $show elevatorpos ++"elevatorpos"
        --let tmp =[]::[Int]
        --let dtmp= elevatorfloor:elevatorpos:tmp
        --hPutStrLn stderr $show dtmp ++"dtmp"
        return (elevatorfloor,elevatorpos)

    
    --hPutStrLn stderr $ "elevator0 = "++ (show ((elevators1!!0)!!1))

    --hPutStrLn stderr $ show eledata ++"elevatorpositions"   
    let gameState = GameState { nbfloors     = n
                              , width          = w
                              , nbrounds      = nb
                              , exitfloor       = exitf
                              , exitpos = exitp
                              , nbtotalclones     = nclones 
                              , nbadditionalelevators  = nbe 
                              , nbelevators =ne
                              , elevators = elevators1
                              , target =t
                              }

    hPutStrLn stderr $ show gameState



    loop gameState

loop :: GameState -> IO ()
loop  gstate = do
    input_line <- getLine
    let input = words input_line
    let clonefloor = read (input!!0) :: Int -- floor of the leading clone
    --hPutStrLn stderr $show clonefloor ++"clonefloor"
    let clonepos = read (input!!1) :: Int -- position of the leading clone on its floor
    --hPutStrLn stderr $show clonepos ++"clonepos"
    let direction = input!!2 -- direction of the leading clone: LEFT or RIGHT
    --hPutStrLn stderr $show direction ++"direction loop"

    let extf= exitfloor gstate
    --hPutStrLn stderr $show extf ++"extf"

    let exs = exitpos gstate

    --if (clonefloor==exitfloor)
    --   then hPutStrLn stderr $show clonefloor
    --   else hPutStrLn stderr "hello" 
    let wall= width gstate
    
    let ev = elevators gstate
    hPutStrLn stderr $show ev ++ "elevators"

    
    let targetpos = target gstate
    --Test 
    
    --let targetpos = if (clonefloor==extf ) then exs else (snd $head $ filter ((==clonefloor).fst) ev)
    let targetpos = if clonefloor == -1 then -1 else (if (clonefloor==extf ) then exs else ((snd $head $ filter ((==clonefloor).fst) ev)))
    hPutStrLn stderr $show clonefloor ++"clonefloor"
    hPutStrLn stderr $show targetpos ++ "targetpos"
    hPutStrLn stderr $show extf ++"exitfloor"

    let ln = length ev
    
    
    hPutStrLn stderr $show ln ++"ln"
    --let b=  (ev!!(clonefloor))!!1
    --hPutStrLn stderr $show b ++ "elevatorpositions"
    hPutStrLn stderr $show clonepos ++"clonepos"
    hPutStrLn stderr $show direction ++"Direction"
    
    --This is code i used to solve all the levels. 

    if (clonefloor >= 0)
    then if (targetpos > clonepos && direction =="LEFT") then putStrLn "BLOCK"
         --else putStrLn "WAIT"
         else if (targetpos < clonepos && direction =="RIGHT") then putStrLn "BLOCK"   
         else putStrLn "WAIT"
    else putStrLn "WAIT"
    

    --if (targetpos > clonepos && direction =="LEFT")
    --            then putStrLn "BLOCK"
    --            else if (targetpos > clonepos && direction =="RIGHT")
    --                then putStrLn "BLOCK"   
    --                else putStrLn "WAIT"
       
  
           

---This below one is working till some levels

    --if (direction == "RIGHT" && clonepos == wall-1)


    --    then putStrLn "BLOCK" 
    --    else if (direction == "LEFT" && clonepos == 0) 
    --    then putStrLn "BLOCK" 

    --    else putStrLn "WAIT"   


 

    loop gstate