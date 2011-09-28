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
  Add(2,1);
  //Sleep(300);
  Add(2,2);
  //Sleep(300);
  Add(2,3);
  Add(2,4);
  Exec("./add3");
  Add(2,5);
  Add(2,6);
  Add(2,7);
  Add(2,8);
  //Sleep(300);
  Add(2,9);
  Add(2,10);
//Exit(1);
  /* not reached */
}
