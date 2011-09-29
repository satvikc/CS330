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
 Add(3, 1);
 /*Add(3, 2);*/
 /*Add(3, 3);*/
 //Sleep(200);
 /*Add(3, 4);*/
 /*Add(3, 5);*/
 /*Add(3, 6);*/
 /*Add(3, 7);*/
 /*Add(3, 8);*/
 /*Add(3, 9);*/
 /*Add(3, 10);*/

//Exit(1);
  /* not reached */
}
