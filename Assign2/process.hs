{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances,PackageImports #-} 
import Data.Maybe (fromJust)
import "mtl" Control.Monad.State
import Data.Char (isDigit)
import Data.List (zipWith5,delete,partition)
import Data.Maybe (fromJust,isNothing,catMaybes)
import Data.Functor
import qualified Data.Map as Map
-- My version of if then else 
if' True a _ = a
if' False _ b = b
preemptive = 0 

getInteger :: String -> IO [Int]

getInteger str = do 
                    content <- readFile str
                    return $ map (\a -> (read a ::Int)) (filter (\a -> a /= "") $ lines content) 

getBursts str = do 
                    content <- readFile str
                    return $ map (map(\a -> (read a ::Int)) . words) $ lines content

-- Some Type aliases 
type PID = Int
type Time = Int
type ReadyQueue = [Process]
type WaitingQueue = [Process]
type Scheduler = ReadyQueue -> (Maybe Process,Time)
type Machine = (Maybe Process , ReadyQueue , WaitingQueue, Time , Schedule)
instance Show Schedule where 
    show s = show $ typ s
-- Some Data Types 
data Schedule = Schedule {
                    typ :: Int ,--Preamptive = 0  or Non Preamptive = 1
                    scheduler :: Scheduler 
                        } 
data Process = Process { 
                 pid :: PID ,
                 arrival :: Time ,
                 burst :: [Time] ,
                 sumBurst :: Time,
                 ioBurst :: [Time] ,
                 priority :: Int,
                 response :: Time ,                          --Initially assign -1
                 sumIOBurst  :: Time ,
                 allotedBurst :: Time,   --Equals time quanta in case of round robin  Assign 0   
                 overAllBurst :: Time     --Initially assign 0 
                        } deriving (Show,Ord)
instance Eq Process where
    (==) a b = pid a == pid b
-- Ready Process puts process to the ready queue , Runscheduler just
-- calls the scheduler . 
totalCpuBurst :: Process -> Time 
totalCpuBurst  = sum.burst 

data Stmt = Ready Process 
            | RunMachine
            | RunScheduler 

--type Machine  is (Running Process ,Ready Queue , cpu clock )

data ProcessStat = ProcessStat {
                            sId :: PID ,
                            sResponse :: Time ,
                            sBurst :: Time ,
                            sIOBurst :: Time,
                            sTurnaroundTime :: Time ,
                            sTick :: Time,
                            sArrival :: Time,
                            sPriority :: Int
                                } deriving (Show)

-- If Process Finishes then it Returns the process otherwise Nothing 

addToQueue :: Process -> ReadyQueue -> ReadyQueue  -- Adds at the end of the queue  . To be implemented
addToQueue a r = a:r
removeFromQueue :: Process -> ReadyQueue -> ReadyQueue -- Note we are not worried if process is not present as we will  ensure it will be present .  To be implemented
removeFromQueue a r = delete a r

reduceBurst ::  Num a => [a] -> [a]
reduceBurst (x:xs) = (x-1:xs) 

updateWaiting :: WaitingQueue -> ReadyQueue -> (ReadyQueue, WaitingQueue)
updateWaiting readyQueue [] = (readyQueue,[])
updateWaiting readyQueue waitingQueue = (rQ ++ readyQueue,wQ)
                                            where f process = process {ioBurst = reduceBurst $ ioBurst process}
                                                  (rQ,wQ) = partition g $ map f waitingQueue
                                                  g process = if' ((head $ ioBurst process) == 0)  True False 

returnResponse :: Time -> Time ->Time-> Time
returnResponse res arrival tick = if' (res<0) (tick-arrival) res 

findProcess sch readyQ tick= do 
                                let (proces,allotedB) = (scheduler sch) readyQ
                                if not $ isNothing proces
                                    then let res = response $ fromJust proces 
                                        in (Just $(fromJust proces) {response = if' (res>=0) res (tick - (arrival $fromJust proces)) , allotedBurst = allotedB}, (removeFromQueue (fromJust proces) readyQ))
                                    else (Nothing , readyQ )

proc :: Stmt -> State Machine (Maybe ProcessStat)
proc (Ready process) =  do
                            (running , readyQueue, waitingQueue, tick , schedule ) <- get 
                            if (not $ isNothing running )
                                then if (typ schedule /= preemptive)
                                         then put $ (running , (addToQueue process readyQueue) ,waitingQueue, tick, schedule )  -- Does not change the cpu tick 
                                         else do 
                                                let readyQ = addToQueue process (addToQueue (fromJust running) readyQueue)
                                                let (process,readyQ') = findProcess schedule readyQ tick
                                                put $ (process , readyQ' ,waitingQueue, tick , schedule)
                                else do 
                                        let readyQ = addToQueue process readyQueue
                                        let (process,readyQ') = findProcess schedule readyQ tick
                                        put $ (process , readyQ' ,waitingQueue, tick , schedule)
                            return Nothing 

proc (RunScheduler) = do 
                            (running , readyQueue ,waitingQueue, tick,schedule ) <- get   -- If scheduler is called ensured that the process will be preempted and not finished 
                            let readyQ = if' (not $ isNothing running ) (addToQueue (fromJust running) readyQueue) readyQueue
                            let (process,readyQ') = findProcess schedule readyQ tick
                            put $ (process , readyQ' ,waitingQueue, tick , schedule)
                            return Nothing 

proc (RunMachine) = do                                                --Runs for a single tick only 
                            (_,_,_,_,schd)<-get
                            if' (typ schd == preemptive) (proc RunScheduler) (return Nothing)
                            (run , readyQ , waitingQ,tick' ,schedule ) <- get 
                            let tick = tick'+1
                            let (readyQueue,waitingQueue) = updateWaiting readyQ waitingQ
                            if not $ isNothing run 
                              then do
                                let running = fromJust run 
                                let cBurst = reduceBurst.burst $ running 
                                let totalBurst = (overAllBurst $running) + 1 
                                let leftBurst = (allotedBurst $ running) - 1
                                if (cBurst /= [0]) then if (head cBurst == 0) then do
                                                                                     let waitingQ' = addToQueue (running {allotedBurst=0,overAllBurst = totalBurst,burst=tail cBurst,response = returnResponse (response running) (arrival running) tick'}) waitingQueue
                                                                                     let (process,readyQ') = findProcess schedule readyQueue tick
                                                                                     put $ (process , readyQ' ,waitingQ', tick , schedule)
                                                                                     return Nothing 
                                                                              else if (leftBurst == 0) then do 
                                                                                                              let readyQ = addToQueue ( running{allotedBurst = 0,overAllBurst = totalBurst,burst=cBurst,response = returnResponse (response running) (arrival running) tick'}) readyQueue
                                                                                                              let (process,readyQ') = findProcess schedule readyQ tick
                                                                                                              put $ (process , readyQ' ,waitingQueue, tick , schedule)
                                                                                                              return Nothing 
                                                                                                       else do
                                                                                                              put $ ((Just $ running {overAllBurst = totalBurst,allotedBurst = leftBurst,burst=cBurst,response = returnResponse (response running) (arrival running) tick'}), readyQueue,waitingQueue , tick , schedule)
                                                                                                              return Nothing 
                                                   else do 
                                                          let (process,readyQ') = findProcess schedule readyQueue tick
                                                          put $ (process , readyQ' ,waitingQueue, tick , schedule)
                                                          return.Just $ ProcessStat { sId = pid running  ,sResponse = returnResponse (response running) (arrival running) tick',sBurst = sumBurst running,sTurnaroundTime = tick - (arrival running), sTick = tick ,sIOBurst = sumIOBurst running,sArrival = arrival running,sPriority = priority running}
                              else do 
                                     let (process,readyQ') = findProcess schedule readyQueue tick
                                     put $ (process , readyQ' ,waitingQueue, tick , schedule)
                                     return Nothing

instance Show (State Machine (Maybe ProcessStat)) where 
    show s = "State Machine (Maybe ProcessStat)"

procs xs = squence $ cprocs xs

cprocs ::  [Process] -> [State Machine (Maybe ProcessStat)]
cprocs xs = process            
                where readyProcesses = map (\s -> proc (Ready s)) xs
                      initList = map (\s -> arrival s) xs
                      cpuBurst = map (\s -> sumBurst s) xs
                      ioB = map (\s -> sumIOBurst s) xs
                      intersperseList = zipWith (\a b -> b-a) initList (tail initList ++ [(last initList) + (sum cpuBurst) + (sum ioB)])
                      createProcessList [] [] = []
                      createProcessList (x:xs) (y:ys) = [x] ++ replicate y (proc (RunMachine)) ++ createProcessList xs ys 
                      process = (createProcessList readyProcesses intersperseList) 

squence ::  (Functor f, Monad f) => [f (Maybe a)] -> f [a]
squence xs = catMaybes <$> sequence xs

processList ::  [Time] -> [[Time]] -> [[Time]] -> [Int] -> [Process]
processList = zipWith5 f [1..] 
                where f count arr bur ioB p = Process { pid = count, arrival = arr , burst = bur, sumBurst = sum bur , ioBurst = ioB ,  priority = p , response = -1 , sumIOBurst = sum ioB , allotedBurst = 0 , overAllBurst =0}

-- First come , first serve algorithm
fcfs [] = (Nothing , 0)
fcfs f = (Just l, head $ burst l) 
            where l = last f

-- Shortest Job First Algorithm
processtuple criteria l = (criteria l,l)
sjf [] = (Nothing , 0)
sjf f = (Just l, remainingBurst l)
            where l = snd leastBurstTuple
                  leastBurstTuple = foldl1 min $ map (processtuple remainingBurst) f
                  remainingBurst = head.burst
-- Round Robin Algorithm
rr [] = (Nothing , 0)
rr f = (Just l, 6::Time)
            where l = last f


-- Priority Based Scheduling Algorithm
ps [] = (Nothing , 0)
ps f = (Just l, head $ burst l)
            where l = snd highestPriorityTuple
                  highestPriorityTuple = foldl1 max $ map (processtuple priority) f
--prog :: [Process] -> [Maybe ProcessStat]

prog p scheduler stmts = fst $ runState (procs stmts) (Nothing,[],[],0,sch)
                where sch = Schedule {typ = p ,
                                    scheduler = scheduler}

scheduler_list = [(fcfs,"fcfs"), (sjf,"sjf") , (rr,"rr"), (ps,"ps")]

--simulator stmts = 
{-initL :: [Int]-}
{-burL :: [[Int]]-}
{-ioL :: [[Int]]-}
{-pL :: [Int]-}
{-initL = [0,10]-}
{-burL = [[1,50],[20]]-}
{-ioL = [[1],[]]-}
{-pL = [2,7]-}

f :: Show a => [a] -> String
f = unlines.map show 

g :: (Show b) => [(a,b)] -> String
g = unlines.map (\(a,b)->show b)

prF ::  (Ord a1, Num a, Num a1) => a1 -> a
prF a | a<5 = 1
      | a<9 = 2
      | otherwise = 3

fn ::  [(Int, Time)] -> [(Integer, Double)]
fn = map (\(a,b)->(a,avg b)) . Map.toList . Map.fromListWith (++) . map (\(a,b)->(prF a,[b])) 

avg ::  Num a => [a] -> Double
avg ls = (read (show $ sum ls) :: Double)/(fromIntegral $ length ls) 

priorityList = map (\f -> (f,[])) [0..9]
simulator prt scheduler scheduler_name stmts = do 
                    let p = prog prt scheduler stmts
                    let prefix = show prt ++ "_" ++ scheduler_name
                    let respList = map (\s -> sResponse s) p
                    let p_respList =fn $ map (\s -> (sPriority s,sResponse s)) p
                    let p_turnaroundList = fn $ map (\s -> (sPriority s,sTurnaroundTime s)) p 
                    let p_waitingList = fn $ map (\s -> (sPriority s,(sTurnaroundTime s) - (sBurst s) -(sIOBurst s))) p
                    let turnaroundList =  map (\s -> sTurnaroundTime s) p 
                    let waitingList =  map (\s -> (sTurnaroundTime s) - (sBurst s) -(sIOBurst s)) p
                    writeFile (prefix ++ "_presponse.txt") (g p_respList)
                    writeFile (prefix ++ "_pturnaround.txt") (g p_turnaroundList)
                    writeFile (prefix ++ "_pwaiting.txt") (g p_waitingList)
                    writeFile (prefix ++ "_response.txt") (f respList)
                    writeFile (prefix ++ "_turnaround.txt") (f turnaroundList)
                    writeFile (prefix ++ "_waiting.txt") (f waitingList)
                    print prefix
                    writeFile (prefix ++ "_avg.txt") (f [avg respList,avg waitingList ,avg turnaroundList ])
                    print p
                    print respList
                    print turnaroundList
                    print waitingList

{-pList = processList initL burL ioL pL-}
{-pr = (head pList) {pid = 5 ,ioBurst = [1]}-}

-- pList = processList initL burL pL
grandSimulator prt stmts = foldl1 (>>) $ map (\(p,s) -> simulator prt p s stmts) scheduler_list
main = do
        initL <- getInteger "arrival_times.txt"
        burL <- getBursts "cpu_bursts.txt"
        ioL <- getBursts "io_bursts.txt"
        pL <- getInteger "priorities.txt"
        let pList = processList initL burL ioL pL
        grandSimulator 0 pList
        grandSimulator 1 pList
