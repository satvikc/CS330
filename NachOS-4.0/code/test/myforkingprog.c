/* add.c
 *	Simple program to test whether the systemcall interface works.
 *	
 *	Just do a add syscall that adds two values and returns the result.
 *
 */

#include "syscall.h"
int main()
{
  int result,newres,pid,p;
  
  SysStats();
  result = Add(42, 20);
  pid=Fork();
  //  Exit(24);
  SysStats();
  if (pid==0)
    Exec2("./add4");
  else
    newres = Add(100,200);
  p = Fork();

  SysStats();
//  Halt();
  /* not reached */
}
