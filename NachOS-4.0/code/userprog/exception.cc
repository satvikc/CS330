// exception.cc 
//	Entry point into the Nachos kernel from user programs.
//	There are two kinds of things that can cause control to
//	transfer back to here from user code:
//
//	syscall -- The user code explicitly requests to call a procedure
//	in the Nachos kernel.  Right now, the only function we support is
//	"Halt".
//
//	exceptions -- The user code does something that the CPU can't handle.
//	For instance, accessing memory that doesn't exist, arithmetic errors,
//	etc.  
//
//	Interrupts (which can also cause control to transfer from user
//	code into the Nachos kernel) are handled elsewhere.
//
// For now, this only handles the Halt() system call.
// Everything else core dumps.
//
// Copyright (c) 1992-1996 The Regents of the University of California.
// All rights reserved.  See copyright.h for copyright notice and limitation 
// of liability and disclaimer of warranty provisions.

#include "copyright.h"
#include "main.h"
#include "syscall.h"
#include "ksyscall.h"
#include "interrupt.h"
#include "thread.h"

#define MAX_FILE_NAME  10

//----------------------------------------------------------------------
// ExceptionHandler
// 	Entry point into the Nachos kernel.  Called when a user program
//	is executing, and either does a syscall, or generates an addressing
//	or arithmetic exception.
//
// 	For system calls, the following is the calling convention:
//
// 	system call code -- r2
//		arg1 -- r4
//		arg2 -- r5
//		arg3 -- r6
//		arg4 -- r7
//
//	The result of the system call, if any, must be put back into r2. 
//
// If you are handling a system call, don't forget to increment the pc
// before returning. (Or else you'll loop making the same system call forever!)
//
//	"which" is the kind of exception.  The list of possible exceptions 
//	is in machine.h.
//----------------------------------------------------------------------
void Add_Syscall()
{
    int result;
    result = SysAdd(/* int op1 */(int)kernel->machine->ReadRegister(4),(int)kernel->machine->ReadRegister(5));
    DEBUG(dbgSys, "Add returning with " << result << "\n");
    kernel->machine->WriteRegister(2, (int)result);
}

void readMemory(int FromUserAddress, char *ToKernelAddress) {
	//Machine::ReadMem(int addr, int size, int *value)
	//DEBUG(dbgSys, "CopyStringToKernel : (FromUserAddress 0x%x) (ToKernelAddress 0x%x)\n", FromUserAddress, ToKernelAddress);
	DEBUG(dbgSys, "CopyStringToKernel");
	int c  /*, physAddr*/ ;
	do {
		//currentThread->space->Translate(FromUserAddress++,&physAddr, 1); //translation
		kernel->machine->ReadMem(FromUserAddress++, 1, &c);   //translation inside ReadMem
		DEBUG(dbgSys, "%c\n"<<(char) c);
		*ToKernelAddress++ = (char) c ;
		//DEBUG('x', "ToKernelAddress (%c)\n", *(ToKernelAddress-1));
	} while (*(ToKernelAddress-1) != NULL);
}

void ExceptionHandler(ExceptionType which)
{
    int type = kernel->machine->ReadRegister(2);
    DEBUG(dbgSys, "Received Exception " << which << " type: " << type << "\n");
    if (which==SyscallException)
    {
        switch(type) 
        {
            case SC_Exec:
                DEBUG(dbgSys, "This is an Exec system call by - "<< kernel->currentThread->getName() << "\n");
                char execfile[MAX_FILE_NAME];
                int memaddress;
                memaddress=kernel->machine->ReadRegister(4);
                readMemory(memaddress,execfile); 
                //func. defined at top of the File
                                Thread * newthread;
                newthread=new Thread("child");
                newthread->space= new AddrSpace;
                newthread->space->Load(execfile);
                DEBUG(dbgSys, "File loaded\n");
                //kernel->scheduler->Run(kernel->currentThread,false);    // ReadyToRun 
                //kernel->machine->WriteRegister(PrevPCReg,kernel->machine->ReadRegister(PCReg));
                //kernel->machine->WriteRegister(PCReg,kernel->machine->ReadRegister(PCReg)+4);
                kernel->currentThread->SaveUserState();
                kernel->currentThread->space->SaveState();
                IntStatus oldLevel;
                oldLevel = kernel->interrupt->SetLevel(IntOff);
                
                newthread->space->InitRegisters();
                kernel->scheduler->Run(newthread,false);
                //kernel->scheduler->Run(newthread,false);
                //assumes that interrupts
                    // are disabled!
                   (void) kernel->interrupt->SetLevel(oldLevel);
                //kernel->currentThread->SaveUserState();     // save the user's 
                //CPU registers
                    //kernel->currentThread->space->SaveState();
                    //kernel->scheduler->Run(newthread,false);
                    //kernel->currentThread->space->Execute();
                    DEBUG(dbgSys, "Did i reach here ??");     
                    /* set previous programm counter (debugging only)*/
                    //kernel->machine->WriteRegister(PrevPCReg, kernel->machine->ReadRegister(PCReg));
                    /* set programm counter to next instruction (all Instructions are 4 byte wide)*/
                    //kernel->machine->WriteRegister(PCReg,0); 
                    /* set next programm counter for brach execution */
                    //newthread->space->RestoreState();
                    //kernel->machine->WriteRegister(NextPCReg, kernel->machine->ReadRegister(PCReg)+4);
                    return;
                    ASSERTNOTREACHED();

                break;

            case SC_Halt:
                DEBUG(dbgSys, "Shutdown, initiated by user program.\n");
                SysHalt();
                 /* set previous programm counter (debugging only)*/
                kernel->machine->WriteRegister(PrevPCReg, kernel->machine->ReadRegister(PCReg));
                /* set programm counter to next instruction (all Instructions are 4 byte wide)*/
                kernel->machine->WriteRegister(PCReg, kernel->machine->ReadRegister(PCReg) + 4);
                /* set next programm counter for brach execution */
                kernel->machine->WriteRegister(NextPCReg, kernel->machine->ReadRegister(PCReg)+4);
                return;
                ASSERTNOTREACHED();
               break;

            case SC_Add:
	            DEBUG(dbgSys, "Add " << kernel->machine->ReadRegister(4) << " + " << kernel->machine->ReadRegister(5) << "\n");
                Add_Syscall();
                  /* set previous programm counter (debugging only)*/
                kernel->machine->WriteRegister(PrevPCReg, kernel->machine->ReadRegister(PCReg));
                /* set programm counter to next instruction (all Instructions are 4 byte wide)*/
                kernel->machine->WriteRegister(PCReg, kernel->machine->ReadRegister(PCReg) + 4);
                /* set next programm counter for brach execution */
                kernel->machine->WriteRegister(NextPCReg, kernel->machine->ReadRegister(PCReg)+4);
                return;
                ASSERTNOTREACHED();
               break;
            case SC_Exit:
               int exitcode;
               exitcode = kernel->machine->ReadRegister(4);
               //kernel->currentThread->Yield();
               DEBUG(dbgSys, "Calling exit " << exitcode); 
                IntStatus oldLevel2;
                oldLevel2 = kernel->interrupt->SetLevel(IntOff);
                

               //kernel->scheduler->toBeDestroyed = kernel->currentThread;
               Thread *nextthread; 
                DEBUG(dbgSys, "Finding next thread to run . ");
               nextthread = kernel->scheduler->FindNextToRun();
              (void) kernel->interrupt->SetLevel(oldLevel2);
               if (!nextthread) {
                   DEBUG(dbgSys, "No more threads to run..");
                    SysHalt();
               }
               oldLevel2 = kernel->interrupt->SetLevel(IntOff);
                DEBUG(dbgSys, "Running next thread . ");
                kernel->scheduler->toBeDestroyed=kernel->currentThread;
               kernel->scheduler->Run(nextthread,false);
               (void) kernel->interrupt->SetLevel(oldLevel2);
               break;
            default:
                cerr << "Unexpected system call " << type << "\n";
                break;
        }
            }
    else 
    {
        cerr << "Unexpected user mode exception" << which << "\nabc " << (int)which << "\n";
    }
    ASSERTNOTREACHED();
}
