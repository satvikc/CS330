/* add.c
 *	Simple program to test whether the systemcall interface works.
 *	
 *	Just do a add syscall that adds two values and returns the result.
 *
 */

#include "syscall.h"

int
main()
{
 int result;
 Add(30,30);
 SysStats();
 Sleep(200);
 Add(5,5);
 SysStats();
result = Add(3, 3);
//Exit(1);
  /* not reached */
}
