#include<syscall.h>
int main()
{int i,j=0;
i =4;
while(j<500)
{
  SWait(5);
  
  SWait(i);
  SWait((i+1)%5);
  SIncrement(i);
  //Eat
  SSignal(i);
  SSignal((i+1)%5);
  SSignal(5);
  //Think
  j++;
}
}
