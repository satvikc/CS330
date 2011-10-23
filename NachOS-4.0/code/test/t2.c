#include<syscall.h>
int main()
{int i,j,k,l=0;
i =2;
while(l<200)
{
  SWait(5);
  
  SWait(i);
  SWait((i+1)%5);
  //P_status(i,1);
  for(k=0;k<50;k++)
  {
    //eating
    j=i;
  }
  //P_status(i,2);
  SSignal(i);
  SSignal((i+1)%5);
  SSignal(5);
  //P_status(i,3);
    for(k=0;k<50;k++)
  {
    //thinking
    j=i;
  }l++;
}
}
