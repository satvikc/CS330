/**************************************************************
 *
 * userprog/ksyscall.h
 *
 * Kernel interface for systemcalls 
 *
 * by Marcus Voelp  (c) Universitaet Karlsruhe
 *
 **************************************************************/

#ifndef __USERPROG_KSYSCALL_H__ 
#define __USERPROG_KSYSCALL_H__ 

#include "kernel.h"


int StartProcess(char path[])
{
    AddrSpace *space = new AddrSpace;
    space->Load(path);
    space->Execute();
}

void SysGetArray(int addr, char cArray[], int size)
{
    int i = 0;
    do
    {
        kernel->machine->ReadMem(int(addr++), 1, (int *)&cArray[i]);
        if ( 0 == cArray[i] ) break;
        i++;
    }while(true);
}

void SysHalt()
{
  kernel->interrupt->Halt();
}


int SysAdd(int op1, int op2)
{
  return op1 + op2;
}






#endif /* ! __USERPROG_KSYSCALL_H__ */
