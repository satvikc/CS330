#include "syscall.h"
//#include "stdio.h"
//#include "stdlib.h"
int main()
{
 int result,newres,pid,a,b,c;
  result  = shared_memory_open(620);
  SysStats();
  Add(1,1);
  pid = Fork();
  Add(pid,0);
  if (pid==0)
  {
  result  = shared_memory_open(620);
  shared_memory_write(0,4,13);
  }
  else
  {
  result = shared_memory_open(620);
  c = shared_memory_read(0, 4);
  Add(5, c);
  shared_memory_close();
  }
  //Halt();
  /* not reached */
}
