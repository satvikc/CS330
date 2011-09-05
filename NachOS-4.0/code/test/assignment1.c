/* add.c
 *	Simple program to test whether the systemcall interface works.
 *	
 *	Just do a add syscall that adds two values and returns the result.
 *
 */

#include "syscall.h"
int main()
{
int i,pid;
    char *progs[] = {"./add4" , "./add2" , "./add3" };
  SysStats();

        pid = Fork();
        if ( pid == 0) Exec2(progs[0]);
        SysStats();
        pid = Fork();
        if ( pid == 0) Exec2(progs[1]);
        SysStats();
        pid = Fork();
        if ( pid == 0) Exec2(progs[2]);
        SysStats();
  /* not reached */
}
