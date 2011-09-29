// alarm.cc
//	Routines to use a hardware timer device to provide a
//	software alarm clock.  For now, we just provide time-slicing.
//
//	Not completely implemented.
//
// Copyright (c) 1992-1996 The Regents of the University of California.
// All rights reserved.  See copyright.h for copyright notice and limitation 
// of liability and disclaimer of warranty provisions.

#include "copyright.h"
#include "alarm.h"
#include "main.h"

//----------------------------------------------------------------------
// Alarm::Alarm
//      Initialize a software alarm clock.  Start up a timer device
//
//      "doRandom" -- if true, arrange for the hardware interrupts to 
//		occur at random, instead of fixed, intervals.
//----------------------------------------------------------------------

Alarm::Alarm(bool doRandom)
{
    timer = new Timer(doRandom, this);
    sclera_counter = 0;
}

//----------------------------------------------------------------------
// Alarm::CallBack
//	Software interrupt handler for the timer device. The timer device is
//	set up to interrupt the CPU periodically (once every TimerTicks).
//	This routine is called each time there is a timer interrupt,
//	with interrupts disabled.
//
//	Note that instead of calling Yield() directly (which would
//	suspend the interrupt handler, not the interrupted thread
//	which is what we wanted to context switch), we set a flag
//	so that once the interrupt handler is done, it will appear as 
//	if the interrupted thread called Yield at the point it is 
//	was interrupted.
//
//	For now, just provide time-slicing.  Only need to time slice 
//      if we're currently running something (in other words, not idle).
//----------------------------------------------------------------------

void 
Alarm::CallBack()
{
    //DEBUG(dbgThread, "Inside CallBack " );
    Interrupt *interrupt = kernel->interrupt;
    MachineStatus status = interrupt->getStatus();

    //if (status == IdleMode) {
        ////DEBUG(dbgThread, "IdleMode");
        //// is it time to quit?
        //if (!interrupt->AnyFutureInterrupts()) {
            //// turn off the timer
        //DEBUG(dbgThread, "AnyFutureInterrupts");
            //timer->Disable();
        //}   //}
    //else
    //{
        // there's someone to preempt
        //DEBUG(dbgThread, "Timer Interrupt");
        interrupt->YieldOnReturn();
		if (!kernel->scheduler->highQueue->IsEmpty() )
            kernel->scheduler->readyList = kernel->scheduler->highQueue;
        else if (!kernel->scheduler->midQueue->IsEmpty() && sclera_counter < 800)
        { 
            sclera_counter+=100;
            kernel->scheduler->readyList = kernel->scheduler->midQueue;
        }
        else if (!kernel->scheduler->lowQueue->IsEmpty() && sclera_counter < 1000)
        {
            sclera_counter+=100;
            kernel->scheduler->readyList = kernel->scheduler->lowQueue;
        }
        else 
        {   
            sclera_counter = 0;
            kernel->scheduler->readyList = kernel->scheduler->highQueue;
        }

		if(!m_qWaitingThreads.empty())
		{
			DEBUG(dbgThread, "[Alarm::CallBack()] Checking to wake the thread: " << m_qWaitingThreads.top().th->getName());
			if(kernel->stats->totalTicks > m_qWaitingThreads.top().wake)
			{
				DEBUG(dbgThread, "[Alarm::CallBack()] Waking up: " << m_qWaitingThreads.top().th->getName());
				kernel->scheduler->ReadyToRun(m_qWaitingThreads.top().th);
				m_qWaitingThreads.pop();
				nr1--;
			}
		}

    //}
        
        //kernel->scheduler->Print();
        


}

//----------------------------------------------------------------------
// Alarm::WaitUntil(int)
// Suspend execution until time > now + ticksNo
//----------------------------------------------------------------------
void
Alarm::WaitUntil(int ticksNo)
{
	Interrupt *interrupt = kernel->interrupt;
    IntStatus oldLevel = interrupt->SetLevel(IntOff);
    
    //-------------------------------------------
    // Push the current thread to waiting thread
    //-------------------------------------------
	WakeThread wt;
	wt.wake = kernel->stats->totalTicks + ticksNo;
	wt.th = kernel->currentThread;
	m_qWaitingThreads.push(wt);
        nr1++;
	
	DEBUG(dbgThread, "[Alarm::WaitUntil(int)] Pushed to waiting queue the thread: " << wt.th->getName());
	//-------------------------------------------
	// Put current thread to sleep
	//-------------------------------------------
//    interrupt->DumpState();
	kernel->currentThread->Sleep(false);
	interrupt->SetLevel(oldLevel);
}
