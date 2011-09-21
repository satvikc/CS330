{-# LANGUAGE FlexibleInstances,PackageImports #-} 
import Data.Maybe (fromJust)
import "mtl" Control.Monad.State

-- My version of if then else 
if' True a _ = a
if' False _ b = b
preemptive = 0 


-- Some Type aliases 
type PID = Int
type Time = Int
type ReadyQueue = [Process]
type Scheduler = ReadyQueue -> (Process,Time)
type Machine = (Process , ReadyQueue , Time , Schedule)

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
                        } deriving (Show)

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
addToQueue _ r = r
removeFromQueue :: Process -> ReadyQueue -> ReadyQueue -- Note we are not worried if process is not present as we will  ensure it will be present .  To be implemented
removeFromQueue _ r = r


proc (Ready process) =  do
                            (running , readyQueue, tick , schedule ) <- get 
                            put $ (running , (addToQueue process readyQueue) , tick, schedule )  -- Does not change the cpu tick 
                            if' (typ schedule == preemptive) (proc RunScheduler) (return Nothing)

proc (RunScheduler) = do 
                            (running , readyQueue , tick,schedule ) <- get   -- If scheduler is called ensured that the process will be preempted and not finished 
                            let readyQ = addToQueue running readyQueue
                            let (process,allotedB) = (scheduler schedule) readyQ
                            put $ (process {allotedBurst = allotedB}, (removeFromQueue process readyQ) , tick , schedule)
                            return Nothing 

proc (RunMachine) = do                                                --Runs for a single tick only 
                            (running , readyQueue , tick ,schedule ) <- get 
                            let totalBurst = (overAllBurst running) + 1 
                            let leftBurst = (allotedBurst running) - 1 
                            if (totalBurst == burst running) then do
                                                            let res = response running 
                                                            put $ (running {response = if' (res>0) res tick , allotedBurst = leftBurst , overAllBurst = totalBurst },readyQueue,tick+1 ,schedule)
                                                            return Nothing
                                                     else do 
                                                                        let (process,allotedB) = (scheduler schedule) readyQueue
                                                                        put $ (process {allotedBurst = allotedB}, (removeFromQueue process readyQueue) , tick+1 , schedule)
                                                                        return.Just $ stat running tick  
                                                        where stat p tick   = ProcessStat { sId = pid p,
                                                                  sResponse = response p , 
                                                                  sBurst = burst p,
                                                                  sTurnaroundTime = (tick+1) - (arrival p) }











                                                    
                            
