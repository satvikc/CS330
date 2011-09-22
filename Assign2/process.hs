{-# LANGUAGE FlexibleInstances,PackageImports #-} 
import Data.Maybe (fromJust)
import "mtl" Control.Monad.State
import Data.Char (isDigit)
import Data.List (zipWith4,delete)
import Data.Maybe (fromJust,isNothing,catMaybes)
import Data.Functor
-- My version of if then else 
if' True a _ = a
if' False _ b = b
preemptive = 0 


-- Some Type aliases 
type PID = Int
type Time = Int
type ReadyQueue = [Process]
type Scheduler = ReadyQueue -> (Maybe Process,Time)
type Machine = (Maybe Process , ReadyQueue , Time , Schedule)

-- Some Data Types 
data Schedule = Schedule {
                    typ :: Int ,--Preamptive = 0  or Non Preamptive = 1
                    scheduler :: Scheduler 
                        }
data Process = Process { 
                 pid :: PID ,
                 arrival :: Time ,
                 burst :: Time ,
                 priority :: Int,
                 response :: Time ,                          --Initially assign -1
                 allotedBurst :: Time,   --Equals time quanta in case of round robin  Assign 0   
                 overAllBurst :: Time     --Initially assign 0 
                        } deriving (Show,Ord)
instance Eq Process where
    (==) a b = pid a == pid b
-- Ready Process puts process to the ready queue , Runscheduler just
-- calls the scheduler . 
data Stmt = Ready Process 
            | RunMachine
            | RunScheduler 

--type Machine  is (Running Process ,Ready Queue , cpu clock )

data ProcessStat = ProcessStat {
                            sId :: PID ,
                            sResponse :: Time ,
                            sBurst :: Time ,
                            sTurnaroundTime :: Time
                                } deriving (Show)

-- If Process Finishes then it Returns the process otherwise Nothing 
proc :: Stmt -> State Machine (Maybe ProcessStat)

addToQueue :: Process -> ReadyQueue -> ReadyQueue  -- Adds at the end of the queue  . To be implemented
addToQueue a r = a:r
removeFromQueue :: Process -> ReadyQueue -> ReadyQueue -- Note we are not worried if process is not present as we will  ensure it will be present .  To be implemented
removeFromQueue a r = delete a r


proc (Ready process) =  do
                            (running , readyQueue, tick , schedule ) <- get 
                            if not $ isNothing running then 
                                    put $ (running , (addToQueue process readyQueue) , tick, schedule )  -- Does not change the cpu tick 
                                else put $ (Just process {response = (tick - arrival process), allotedBurst = burst process}, readyQueue , tick , schedule)
                            if' (typ schedule == preemptive) (proc RunScheduler) (return Nothing)

proc (RunScheduler) = do 
                            (running , readyQueue , tick,schedule ) <- get   -- If scheduler is called ensured that the process will be preempted and not finished 
                            let readyQ = if' (not $ isNothing running ) (addToQueue (fromJust running) readyQueue) readyQueue
                            let (process,allotedB) = (scheduler schedule) readyQ
                            if not $ isNothing process
                                        then let res = response $ fromJust process 
                                              in put $ (Just $(fromJust process) {response = if' (res>=0) res (tick - (arrival $fromJust process)) , allotedBurst = allotedB}, (removeFromQueue (fromJust process) readyQ) , tick , schedule)
                                    else put $ (Nothing , readyQ , tick , schedule)
                            return Nothing 

proc (RunMachine) = do                                                --Runs for a single tick only 
                            (running , readyQueue , tick ,schedule ) <- get 
                            if not $ isNothing running 
                              then do  
                                let totalBurst = (overAllBurst $ fromJust running) + 1 
                                let leftBurst = (allotedBurst $ fromJust running) - 1
                                if ((totalBurst /= (burst $ fromJust running))) then if (leftBurst == 0) then do
                                                                                        let readyQ = addToQueue ((fromJust running) {allotedBurst = leftBurst,overAllBurst = totalBurst}) readyQueue
                                                                                        let (process,allotedB) = (scheduler schedule) readyQ
                                                                                        put $ ((Just $ (fromJust process) {allotedBurst = allotedB}), (removeFromQueue (fromJust process) readyQ) , tick+1 , schedule)
                                                                                        return Nothing 
                                                                                                              else do 
                                                                                                                    let res = response $ fromJust running 
                                                                                                                    put $ (Just $ (fromJust running) {response = if' (res>=0) res (tick - (arrival $fromJust running)) , allotedBurst = leftBurst , overAllBurst = totalBurst },readyQueue,tick+1 ,schedule)
                                                                                                                    return Nothing
                                                                                else do 
                                                                                            let (process,allotedB) = (scheduler schedule) readyQueue
                                                                                            if not $ isNothing process 
                                                                                                then put $ ((Just $ (fromJust process) {allotedBurst = allotedB}), (removeFromQueue (fromJust process) readyQueue) , tick+1 , schedule)
                                                                                                else put $ (Nothing , readyQueue , tick+1 , schedule)
                                                                                            return.Just $ ProcessStat { sId = pid $ fromJust running,sResponse = response $ fromJust running , sBurst = burst $ fromJust running,sTurnaroundTime = (tick+1) - (arrival $ fromJust running) }
                              else do 
                                    let (process,allotedB) = (scheduler schedule) readyQueue
                                    if not $ isNothing process
                                        then let res = response $ fromJust process
                                                  in put $ ((Just $ (fromJust process) {response = if' (res>=0) res (tick - (arrival $fromJust process)) , allotedBurst = allotedB-1,overAllBurst = (overAllBurst $ fromJust process) +1  }), (removeFromQueue (fromJust process) readyQueue) , tick+1 , schedule)
                                         else put $ (Nothing , readyQueue , tick+1 , schedule)
                                    return Nothing

getInteger :: String -> IO [Int]

getInteger str = do 
                    content <- readFile str
                    return $ map (\a -> (read a ::Int)) (filter (\a -> a /= "") $ lines content) 

--procs :: [Process] -> State Machine [ProcessStat]


procs xs = squence process
            where readyProcesses = map (\s -> proc (Ready s)) xs
                  initList = map (\s -> arrival s) xs
                  cpuBurst = map (\s -> burst s) xs
                  intersperseList = zipWith (\a b -> b-a) initList (tail initList ++ [(last initList) + (sum cpuBurst)])
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
fcfs f = (Just l, burst l) 
            where l = last f

-- Shortest Job First Algorithm
processtuple criteria l = (criteria l,l)

sjf [] = (Nothing , 0)
sjf f = (Just l, burst l)
            where l = snd leastBurstTuple
                  leastBurstTuple = foldl1 min $ map (processtuple burst) f
-- Round Robin Algorithm
rr [] = (Nothing , 0)
rr f = (Just l, 5::Time)
            where l = last f


-- Priority Based Scheduling Algorithm
ps [] = (Nothing , 0)
ps f = (Just l, burst l)
            where l = snd highestPriorityTuple
                  highestPriorityTuple = foldl1 max $ map (processtuple priority) f
--prog :: [Process] -> [Maybe ProcessStat]

prog preemptive scheduler stmts = fst $ runState (procs stmts) (Nothing,[],0,sch)
                where sch = Schedule {typ = preemptive ,
                                    scheduler = scheduler}

scheduler_list = [(fcfs,"fcfs"), (sjf,"sjf") , (rr,"rr"), (ps,"ps")]

--simulator stmts = 
initL :: [Int]
burL :: [Int]
pL :: [Int]
initL = [0,0,0,0]
burL = [5,20,30,10]
pL = [2,4,1,6]

f = unlines.map show 
simulator preemptive scheduler scheduler_name stmts = do 
                    let p = prog preemptive scheduler stmts
                    let prefix = show preemptive ++ "_" ++ scheduler_name
                    let respList = map (\s -> sResponse s) p 
                    let turnaroundList =  map (\s -> sTurnaroundTime s) p 
                    let waitingList =  map (\s -> (sTurnaroundTime s) - (sBurst s)) p
                    writeFile (prefix ++ "_response.txt") (f respList)
                    writeFile (prefix ++ "_turnaround.txt") (f turnaroundList)
                    writeFile (prefix ++ "_waiting.txt") (f waitingList)
                    print p
                    print respList
                    print turnaroundList
                    print waitingList

pList = processList initL burL pL
grandSimulator preemptive= foldl1 (>>) $ map (\(p,s) -> simulator preemptive p s pList) scheduler_list
main = do
        let pList = processList initL burL pL
        grandSimulator 0
        grandSimulator 1
