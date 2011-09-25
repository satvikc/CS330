/* add.c
 *	Simple program to test whether the systemcall interface works.
 *	
 *	Just do a add syscall that adds two values and returns the result.
 *
 */

#include "syscall.h"
int main()
{
  int result,newres,pid,p,a,b,c;
  
  SysStats();
  result = Add(42, 20);
  pid=Fork();
  a = Fork();
  b = Fork();
  c = Fork();
  //  Exit(24);
//  SysStats();
//  if (pid==0)
//    Exec2("./add4");
//  else
//    newres = Add(100,200);
  Add(1,1);
  p = Fork();
  Add(0,0);

  SysStats();
//  Halt();
  /* not reached */
}
