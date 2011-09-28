/* add.c
 *	Simple program to test whether the systemcall interface works.
 *	
 *	Just do a add syscall that adds two values and returns the result.
 *
 */

#include "syscall.h"
#include "stdio.h"
#include "stdlib.h"
int main()
{
  int result,newres,pid,a,b,c;
  SysStats();
  Add(1,1);
  pid = Fork();
  Add(pid,0);
  a = Fork();
//  b = Fork();
//  c = Fork();
  if (pid==0)
  Exec2("./add2");
  //  Exit(24);
  //Sleep(500);
  newres = Add(10,20);
  SysStats();
//  Halt();
  //Exit(0);
  /* not reached */
}
