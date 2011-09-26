{-# LANGUAGE FlexibleInstances,PackageImports #-} 
import Data.Maybe (fromJust)
import "mtl" Control.Monad.State
import Data.Char (isDigit)
import Data.List (zipWith4,delete,partition)
import Data.Maybe (fromJust,isNothing,catMaybes)
import Data.Functor
-- My version of if then else 
if' True a _ = a
if' False _ b = b
preemptive = 0 

getInteger :: String -> IO [Int]

getInteger str = do 
                    content <- readFile str
                    return $ map (\a -> (read a ::Int)) (filter (\a -> a /= "") $ lines content) 


-- Some Type aliases 
type PID = Int
type Time = Int
type ReadyQueue = [Process]
type WaitingQueue = [Process]
type Scheduler = ReadyQueue -> (Maybe Process,Time)
type Machine = (Maybe Process , ReadyQueue , WaitingQueue, Time , Schedule)

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
                            sTick :: Time
                                } deriving (Show)

-- If Process Finishes then it Returns the process otherwise Nothing 
proc :: Stmt -> State Machine (Maybe ProcessStat)

addToQueue :: Process -> ReadyQueue -> ReadyQueue  -- Adds at the end of the queue  . To be implemented
addToQueue a r = a:r
removeFromQueue :: Process -> ReadyQueue -> ReadyQueue -- Note we are not worried if process is not present as we will  ensure it will be present .  To be implemented
removeFromQueue a r = delete a r

reduceBurst (x:xs) = (x-1:xs) 

updateWaiting :: WaitingQueue->ReadyQueue->(ReadyQueue,WaitingQueue)
updateWaiting readyQueue waitingQueue = (rQ ++ readyQueue,wQ)
                                            where f process = process {ioBurst = reduceBurst $ ioBurst process}
                                                  (rQ,wQ) = partition g $ map f waitingQueue
                                                  g process = if' ((head $ ioBurst process) == 0)  True False 

returnResponse :: Time -> Time -> Time
returnResponse res tick = if' (res<0) tick res 

proc (Ready process) =  do
                            (running , readyQueue, waitingQueue, tick , schedule ) <- get 
                            if not $ isNothing running then 
                                    put $ (running , (addToQueue process readyQueue) ,waitingQueue, tick, schedule )  -- Does not change the cpu tick 
                                else do 
                                        let readyQ = addToQueue process readyQueue
                                        let (proces,allotedB) = (scheduler schedule) readyQ
                                        if not $ isNothing proces
                                            then let res = response $ fromJust proces 
                                              in put $ (Just $(fromJust proces) {response = if' (res>=0) res (tick - (arrival $fromJust proces)) , allotedBurst = allotedB}, (removeFromQueue (fromJust proces) readyQ) ,waitingQueue, tick , schedule)
                                            else put $ (Nothing , readyQ ,waitingQueue, tick , schedule)
                            if' (typ schedule == preemptive) (proc RunScheduler) (return Nothing)

proc (RunScheduler) = do 
                            (running , readyQueue ,waitingQueue, tick,schedule ) <- get   -- If scheduler is called ensured that the process will be preempted and not finished 
                            let readyQ = if' (not $ isNothing running ) (addToQueue (fromJust running) readyQueue) readyQueue
                            let (process,allotedB) = (scheduler schedule) readyQ
                            if not $ isNothing process
                                        then let res = response $ fromJust process 
                                              in put $ (Just $(fromJust process) {response = if' (res>=0) res (tick - (arrival $fromJust process)) , allotedBurst = allotedB}, (removeFromQueue (fromJust process) readyQ) ,waitingQueue, tick , schedule)
                                    else put $ (Nothing , readyQ ,waitingQueue, tick , schedule)
                            return Nothing 

proc (RunMachine) = do                                                --Runs for a single tick only 
                            (_, _ , _,_,sch ) <- get 
                            _ <- if' (typ sch == preemptive) (proc RunScheduler) (return Nothing)
                            (running , readyQ , waitingQ,tick' ,schedule ) <- get 
                            let tick = tick'+1
                            let (readyQueue,waitingQueue) = updateWaiting waitingQueue readyQueue
                            if not $ isNothing running 
                              then do  
                                let cBurst = reduceBurst.burst $ fromJust running 
                                let totalBurst = (overAllBurst $ fromJust running) + 1 
                                let leftBurst = (allotedBurst $ fromJust running) - 1
                                if ((totalBurst < (sumBurst $ fromJust running))) then if (head cBurst == 0) 
                                                                                   then do
                                                                                        let waitingQ' = addToQueue ((fromJust running) {allotedBurst = leftBurst,overAllBurst = totalBurst,burst=tail cBurst,response = returnResponse (response $ fromJust running) tick}) waitingQueue
                                                                                        let (process,allotedB) = (scheduler schedule) readyQueue
                                                                                        if not $ isNothing process 
                                                                                            then put $ ((Just $ (fromJust process) {allotedBurst = allotedB,response = returnResponse (response $ fromJust process) tick}), (removeFromQueue (fromJust process) readyQueue) ,waitingQ', tick , schedule)
                                                                                            else put $ (Nothing , readyQueue ,waitingQueue, tick , schedule)
                                                                                        return Nothing 
                                                                                    else if (leftBurst == 0)
                                                                                            then do 
                                                                                                    let readyQ' = addToQueue ((fromJust running) {allotedBurst = leftBurst,overAllBurst = totalBurst,burst=cBurst,response = returnResponse (response $ fromJust running) tick}) readyQueue
                                                                                                    let (process,allotedB) = (scheduler schedule) readyQ'
                                                                                                    if not $ isNothing process 
                                                                                                        then put $ ((Just $ (fromJust process) {allotedBurst = allotedB,response = returnResponse (response $ fromJust process) tick}), (removeFromQueue (fromJust process) readyQ') ,waitingQueue, tick , schedule)
                                                                                                        else put $ (Nothing , readyQueue ,waitingQueue, tick , schedule)
                                                                                                    return Nothing 
                                                                                             else do
                                                                                                        put $ ((Just $ (fromJust running) {overAllBurst = totalBurst,allotedBurst = leftBurst,burst=cBurst,response = returnResponse (response $ fromJust running) tick}), readyQueue,waitingQueue , tick , schedule)
                                                                                                        return Nothing 
                                                                                else do 
                                                                                            let (process,allotedB) = (scheduler schedule) readyQueue
                                                                                            if not $ isNothing process 
                                                                                                then put $ ((Just $ (fromJust process) {allotedBurst = allotedB,response = returnResponse (response $ fromJust process) tick}), (removeFromQueue (fromJust process) readyQueue) ,waitingQueue, tick , schedule)
                                                                                                else put $ (Nothing , readyQueue ,waitingQueue, tick , schedule)
                                                                                            return.Just $ ProcessStat { sId = pid $ fromJust running  ,sResponse = returnResponse (response $ fromJust running) tick,sBurst = sumBurst $ fromJust running,sTurnaroundTime = tick - (arrival $ fromJust running), sTick = tick ,sIOBurst = sumIOBurst $ fromJust running}
                              else do 
                                     _ <- (proc RunScheduler)
                                     proc RunMachine

--procs :: [Process] -> State Machine [ProcessStat]
-- Num procs = 5 with no . of  cpu bursts = 50  

procs xs = squence process
            where readyProcesses = map (\s -> proc (Ready s)) xs
                  initList = map (\s -> arrival s) xs
                  cpuBurst = map (\s -> burst s) xs
                  intersperseList = zipWith (\a b -> b-a) initList (tail initList ++ [(last initList) + (sum $ map sum cpuBurst)])
                  createProcessList [] [] = []
                  createProcessList (x:xs) (y:ys) = [x] ++ replicate y (proc (RunMachine)) ++ createProcessList xs ys 
                  process = (createProcessList readyProcesses intersperseList) 
squence xs = catMaybes <$> sequence xs
--squence = foldr mcons (return [])
--  where
--    mcons p q = p >>= \x -> q >>= \y -> if' (isNothing x) (return y) (return (x : y))
processList = zipWith4 f [1..] 
                where f count arr bur p = Process { pid = count, arrival = arr , burst = bur, priority = p , response = -1 , allotedBurst = 0 , overAllBurst =0} 

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

prog preemptive scheduler stmts = fst $ runState (procs stmts) (Nothing,[],[],0,sch)
                where sch = Schedule {typ = preemptive ,
                                    scheduler = scheduler}

scheduler_list = [(fcfs,"fcfs"), (sjf,"sjf") , (rr,"rr"), (ps,"ps")]

--simulator stmts = 
-- initL :: [Int]
-- burL :: [Int]
-- pL :: [Int]
--initL = [0,10,20,25]
--burL = [5,35,30,10]
--pL = [2,4,1,6]
f :: Show a => [a] -> String
f = unlines.map show 
simulator preemptive scheduler scheduler_name stmts = do 
                    let p = prog preemptive scheduler stmts
                    let prefix = show preemptive ++ "_" ++ scheduler_name
                    let respList = map (\s -> sResponse s) p
                    let avg ls = (read (show $ sum ls) :: Double)/(fromIntegral $ length ls) 
                    let turnaroundList =  map (\s -> sTurnaroundTime s) p 
                    let waitingList =  map (\s -> (sTurnaroundTime s) - (sBurst s)) p
                    writeFile (prefix ++ "_response.txt") (f respList)
                    writeFile (prefix ++ "_turnaround.txt") (f turnaroundList)
                    writeFile (prefix ++ "_waiting.txt") (f waitingList)
                    print prefix
                    writeFile (prefix ++ "_avg.txt") (f [avg respList,avg waitingList ,avg turnaroundList ])
                    print p
                    print respList
                    print turnaroundList
                    print waitingList

-- pList = processList initL burL pL
grandSimulator preemptive stmts = foldl1 (>>) $ map (\(p,s) -> simulator preemptive p s stmts) scheduler_list
main = do
        initL <- getInteger "arrival_times.txt"
        burL <- getInteger "burst_times.txt"
        pL <- getInteger "priorities.txt"
        let pList = processList initL burL pL
        grandSimulator 0 pList
        grandSimulator 1 pList
