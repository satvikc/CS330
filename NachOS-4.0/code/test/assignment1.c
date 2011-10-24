/* halt.c
 *	Simple program to test whether running a user program works.
 *	
 *	Just do a "syscall" that shuts down the OS.
 *
 * 	NOTE: for some reason, user programs with global data structures 
 *	sometimes haven't worked in the Nachos environment.  So be careful
 *	out there!  One option is to allocate data structures as 
 * 	automatics within a procedure, but if you do this, you have to
 *	be careful to allocate a big enough stack to hold the automatics!
 */

#include "syscall.h"
//Note: Since default priority is 9 this program will rum and create processes whithout being preempted in between
int
main()
{
//this section for calling the randomly generated file t0.c - t39.c

/*Note: If you try to make more processes in Nachos than it can handle it will show segmentation fault or it will terminate
because of some Assertion statement.
Running 10 processes will not create any problem.
*/
/*Test method 1:
	int i;
 	char str[20];
 	str[0] = '.';str[1] ='.';str[2] = '/';str[3] = 't';str[4] = 'e';str[5] = 's';str[6] = 't';str[7] = '/';str[8] = 't';
 	for(i=0;i<10;i++)  //Do not run this for loop for more than 10 times
 	{
 	  str[9] = (char)((i%10)+48);
 	  if((i/10)!=0){str[10] = (char)((i/10)+48);str[11]='\0';}
 	  else str[10]='\0';
 	  Exec(str,i%10);
 	}
 	SysStats();

*/	
//Test method 2:
//use this section for creating processes along with priority of your choice 
	int table,p0,p1,p2,p3,p4;
	
	p0 = SCreate(1);
	p1 = SCreate(1);
	p2 = SCreate(1);
	p3 = SCreate(1);
	p4 = SCreate(1);
	table = SCreate(4);
	//Exec("../test/add",9);
  	Exec("../test/t0",8);
  	Exec("../test/t1",8);
	Exec("../test/t2",8);
	Exec("../test/t3",8);
	Exec("../test/t4",8);
//  	Exec("../test/add",9);
//   	Exec("../test/add",6);
//   	Exec("../test/t0",5);
//   	Exec("../test/t0",6);
//  	Exec("../test/t0",9);
//  	Exec("../test/add",9);
// 	
//  	SysStats();

  
}
