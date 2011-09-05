/* add.c
 *	Simple program to test whether the systemcall interface works.
 *	
 *	Just do a add syscall that adds two values and returns the result.
 *
 */

#include "syscall.h"
int main()
{
int i;
    char *progs[] = {"./add4" , "./add2" , "./add3" };
  SysStats();
    for ( i = 0 ; i < 3 ; i++ )
    {
        Exec(progs[i]);
        SysStats();
    }
  /* not reached */
}
