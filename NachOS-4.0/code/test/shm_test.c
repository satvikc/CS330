#include "syscall.h"
//#include "stdio.h"
//#include "stdlib.h"
int main()
{
 int newres,pid,a,b,c;
 int *result,*res;
  //result  = shared_memory_open(720);
  SysStats();
  Add(1,1);
  //Add(pid,0);
  //result  = shared_memory_open(520);
  //a = Fork();
//  b = Fork();
//  c = Fork();
  //if (pid==0)
  //Exec2("./add2");
  //  Exit(24);
  //Sleep(500);
  result  = shared_memory_open(520);
  result = 10;
  //newres = Add(10,20);
  pid = Fork();
 // SysStats();
//  Halt();
  //Exit(0);
  if (pid == 0)
  {
      res = shared_memory_open(620);
      //Add(5,*res);
  }
  shared_memory_close();
  Halt();
  /* not reached */
}
