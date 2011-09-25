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
  result = Add(2,2);
  //Sleep(3);
  Exec("./add3");
//Exit(1);
  /* not reached */
}
