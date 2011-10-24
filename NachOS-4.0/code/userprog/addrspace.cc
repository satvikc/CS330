// addrspace.cc 
//	Routines to manage address spaces (executing user programs).
//
//	In order to run a user program, you must:
//
//	1. link with the -n -T 0 option 
//	2. run coff2noff to convert the object file to Nachos format
//		(Nachos object code format is essentially just a simpler
//		version of the UNIX executable object code format)
//	3. load the NOFF file into the Nachos file system
//		(if you are using the "stub" file system, you
//		don't need to do this last step)
//
// Copyright (c) 1992-1996 The Regents of the University of California.
// All rights reserved.  See copyright.h for copyright notice and limitation 
// of liability and disclaimer of warranty provisions.

#include "copyright.h"
#include "main.h"
#include "addrspace.h"
#include "machine.h"
#include "noff.h"
#include "bitmap.h"
//----------------------------------------------------------------------
// SwapHeader
// 	Do little endian to big endian conversion on the bytes in the 
//	object file header, in case the file was generated on a little
//	endian machine, and we're now running on a big endian machine.
//----------------------------------------------------------------------

const int N=NumPhysPages/2;
Bitmap n(NumPhysPages);

static void 
SwapHeader (NoffHeader *noffH)
{
    noffH->noffMagic = WordToHost(noffH->noffMagic);
    noffH->code.size = WordToHost(noffH->code.size);
    noffH->code.virtualAddr = WordToHost(noffH->code.virtualAddr);
    noffH->code.inFileAddr = WordToHost(noffH->code.inFileAddr);
#ifdef RDATA
    noffH->readonlyData.size = WordToHost(noffH->readonlyData.size);
    noffH->readonlyData.virtualAddr = 
           WordToHost(noffH->readonlyData.virtualAddr);
    noffH->readonlyData.inFileAddr = 
           WordToHost(noffH->readonlyData.inFileAddr);
#endif 
    noffH->initData.size = WordToHost(noffH->initData.size);
    noffH->initData.virtualAddr = WordToHost(noffH->initData.virtualAddr);
    noffH->initData.inFileAddr = WordToHost(noffH->initData.inFileAddr);
    noffH->uninitData.size = WordToHost(noffH->uninitData.size);
    noffH->uninitData.virtualAddr = WordToHost(noffH->uninitData.virtualAddr);
    noffH->uninitData.inFileAddr = WordToHost(noffH->uninitData.inFileAddr);

#ifdef RDATA
    DEBUG(dbgAddr, "code = " << noffH->code.size <<  
                   " readonly = " << noffH->readonlyData.size <<
                   " init = " << noffH->initData.size <<
                   " uninit = " << noffH->uninitData.size << "\n");
#endif
}

//----------------------------------------------------------------------
// AddrSpace::AddrSpace
// 	Create an address space to run a user program.
//	Set up the translation from program memory to physical 
//	memory.  For now, this is really simple (1:1), since we are
//	only uniprogramming, and we have a single unsegmented page table
//----------------------------------------------------------------------

AddrSpace::AddrSpace()
{/*
    pageTable = new TranslationEntry[N];
    for (int i = 0; i < N; i++) {
	pageTable[i].virtualPage = i;	// for now, virt page # = phys page #
	pageTable[i].physicalPage = i+10;
	pageTable[i].valid = TRUE;
	pageTable[i].use = FALSE;
	pageTable[i].dirty = FALSE;
	pageTable[i].readOnly = FALSE;  
	for(int j=0;j < PageSize;j++)
		kernel->machine->mainMemory[(i+10)*PageSize+j]=0;
    }
   */ 
    // zero out the entire address space

}
AddrSpace::AddrSpace(AddrSpace &space)
{
	numPages =space.numPages;
	//failed = false;
	int num=numPages;

	//if(num > bm->NumClear())
	//{
	//	failed=true;
	//	numPages=0;
	//	throw(PageLack);
	//	return;
	//}
	pageTable = new TranslationEntry[numPages] ;

#ifndef USE_TLB
	unsigned int i,j;
    int c;
    int x;
	for (i = 0; i < numPages-1; i++) {
		pageTable[i].virtualPage = i;
        x = n.FindAndSet();
		pageTable[i].physicalPage = x;
        if (i==0){
            this->id = pageTable[0].physicalPage;
        }
		pageTable[i].valid = space.pageTable[i].valid;
		pageTable[i].use = space.pageTable[i].use;
		pageTable[i].dirty = space.pageTable[i].dirty;
		pageTable[i].readOnly = space.pageTable[i].readOnly;
        for (j = 0; j < PageSize ; j++)
        {
            kernel->machine->ReadMem(j+i*PageSize,1,&c);
            kernel->machine->mainMemory[x*PageSize+j] = (char)c;

        }

	}

    pageTable[numPages-1].virtualPage = numPages-1;
    int k = n.FindAndSet();
    pageTable[numPages-1].physicalPage = k;
    pageTable[numPages-1].valid = TRUE;
    pageTable[numPages-1].use = FALSE;
    pageTable[numPages-1].dirty = TRUE;
    pageTable[numPages-1].readOnly = FALSE; 
    this->idsh = pageTable[numPages-1].physicalPage;



#endif
}


//----------------------------------------------------------------------
// AddrSpace::~AddrSpace
// 	Dealloate an address space.
//----------------------------------------------------------------------

AddrSpace::~AddrSpace()
{
    int k;
	for (unsigned int i = 0; i < numPages; i++)
    {
		k=pageTable[i].physicalPage;
	    n.Clear(k);
    }
   delete pageTable;
}


//----------------------------------------------------------------------
// AddrSpace::Load
// 	Load a user program into memory from a file.
//
//	Assumes that the page table has been initialized, and that
//	the object code file is in NOFF format.
//
//	"fileName" is the file containing the object code to load into memory
//----------------------------------------------------------------------

bool 
AddrSpace::Load(char *fileName) 
{
    OpenFile *executable = kernel->fileSystem->Open(fileName);
    NoffHeader noffH;
    unsigned int size;

    if (executable == NULL) {
	cerr << "Unable to open file " << fileName << "\n";
	return FALSE;
    }

    executable->ReadAt((char *)&noffH, sizeof(noffH), 0);
    if ((noffH.noffMagic != NOFFMAGIC) && 
		(WordToHost(noffH.noffMagic) == NOFFMAGIC))
    	SwapHeader(&noffH);
    ASSERT(noffH.noffMagic == NOFFMAGIC);

#ifdef RDATA
// how big is address space?
    size = noffH.code.size + noffH.readonlyData.size + noffH.initData.size +
           noffH.uninitData.size + UserStackSize;	
                                                // we need to increase the size
						// to leave room for the stack
#else
// how big is address space?
    size = noffH.code.size + noffH.initData.size + noffH.uninitData.size 
			+ UserStackSize;	// we need to increase the size
						// to leave room for the stack
#endif
    numPages = divRoundUp(size, PageSize);
    numPages += 1;
    size = numPages * PageSize;

    ASSERT(numPages <= NumPhysPages);		// check we're not trying
						// to run anything too big --
						// at least until we have
						// virtual memory

    DEBUG(dbgAddr, "Initializing address space: " << numPages << ", " << size);
    
    pageTable = new TranslationEntry[numPages];
    for (int i = 0; i < numPages-1; i++) {
	pageTable[i].virtualPage = i;	// for now, virt page # = phys page #
	int k=n.FindAndSet();
	pageTable[i].physicalPage = k;
    if (i==0){
        this->id = pageTable[0].physicalPage;
    }
    DEBUG(dbgAddr, "physical address "<< k);
	pageTable[i].valid = TRUE;
	pageTable[i].use = FALSE;
	pageTable[i].dirty = FALSE;
	pageTable[i].readOnly = FALSE;  
	for(int j=0;j < PageSize;j++)
		kernel->machine->mainMemory[(k)*PageSize+j]=0;
    }
 
    pageTable[numPages-1].virtualPage = numPages-1;
	int k = n.FindAndSet();
	pageTable[numPages-1].physicalPage = k;
	pageTable[numPages-1].valid = TRUE;
	pageTable[numPages-1].use = FALSE;
	pageTable[numPages-1].dirty = TRUE;
	pageTable[numPages-1].readOnly = FALSE; 
	DEBUG(dbgSys, "physical page "<< k << " virtual page" << numPages-1 << " for sharing"<<endl);
	for(int j=0;j < PageSize;j++)
		kernel->machine->mainMemory[(k)*PageSize+j]=0;
	this->idsh = pageTable[numPages-1].physicalPage;
	this->share = FALSE;
	
// then, copy in the code and data segments into MemorySize
// Note: this code assumes that virtual address = physical address
    if (noffH.code.size > 0) {
		int	j=(pageTable[noffH.code.virtualAddr/PageSize].physicalPage)*PageSize + ((noffH.code.virtualAddr)%PageSize);
        DEBUG(dbgAddr, "Initializing code segment.");
	DEBUG(dbgAddr, noffH.code.virtualAddr << ", " << noffH.code.size<<","<<j);
        executable->ReadAt(
		&(kernel->machine->mainMemory[j]), 
			noffH.code.size, noffH.code.inFileAddr);
    }
    if (noffH.initData.size > 0) {
		int	j=(pageTable[noffH.initData.virtualAddr/PageSize].physicalPage)*PageSize + ((noffH.initData.virtualAddr)%PageSize);
        DEBUG(dbgAddr, "Initializing data segment.");
	DEBUG(dbgAddr, noffH.initData.virtualAddr << ", " << noffH.initData.size);
        executable->ReadAt(
		&(kernel->machine->mainMemory[j]),
			noffH.initData.size, noffH.initData.inFileAddr);
    }

#ifdef RDATA
    if (noffH.readonlyData.size > 0) {
		int	j=(pageTable[noffH.readonlyData.virtualAddr/PageSize].physicalPage)*PageSize + ((noffH.readonlyData.virtualAddr)%PageSize);
        DEBUG(dbgAddr, "Initializing read only data segment.");
	DEBUG(dbgAddr, noffH.readonlyData.virtualAddr << ", " << noffH.readonlyData.size);
        executable->ReadAt(
		&(kernel->machine->mainMemory[j]),
			noffH.readonlyData.size, noffH.readonlyData.inFileAddr);
    }
#endif

    delete executable;			// close file
    return TRUE;			// success
}

//----------------------------------------------------------------------
// AddrSpace::Execute
// 	Run a user program using the current thread
//
//      The program is assumed to have already been loaded into
//      the address space
//
//----------------------------------------------------------------------

void 
AddrSpace::Execute() 
{

    kernel->currentThread->space = this;

    this->InitRegisters();		// set the initial register values
    this->RestoreState();		// load page table register

    kernel->machine->Run();		// jump to the user progam

    ASSERTNOTREACHED();			// machine->Run never returns;
					// the address space exits
					// by doing the syscall "exit"
}


//----------------------------------------------------------------------
// AddrSpace::InitRegisters
// 	Set the initial values for the user-level register set.
//
// 	We write these directly into the "machine" registers, so
//	that we can immediately jump to user code.  Note that these
//	will be saved/restored into the currentThread->userRegisters
//	when this thread is context switched out.
//----------------------------------------------------------------------

void
AddrSpace::InitRegisters()
{
    Machine *machine = kernel->machine;
    int i;

    for (i = 0; i < NumTotalRegs; i++)
	machine->WriteRegister(i, 0);

    // Initial program counter -- must be location of "Start", which
    //  is assumed to be virtual address zero
    machine->WriteRegister(PCReg, 0);	

    // Need to also tell MIPS where next instruction is, because
    // of branch delay possibility
    // Since instructions occupy four bytes each, the next instruction
    // after start will be at virtual address four.
    machine->WriteRegister(NextPCReg, 4);

   // Set the stack register to the end of the address space, where we
   // allocated the stack; but subtract off a bit, to make sure we don't
   // accidentally reference off the end!
    machine->WriteRegister(StackReg, numPages * PageSize - 16);
    DEBUG(dbgAddr, "Initializing stack pointer: " << numPages * PageSize - 16);
}

//----------------------------------------------------------------------
// AddrSpace::SaveState
// 	On a context switch, save any machine state, specific
//	to this address space, that needs saving.
//
//	For now, don't need to save anything!
//----------------------------------------------------------------------

void AddrSpace::SaveState() 
{
    pageTable = kernel->machine->pageTable ;
    numPages  = kernel->machine->pageTableSize;
}

//----------------------------------------------------------------------
// AddrSpace::RestoreState
// 	On a context switch, restore the machine state so that
//	this address space can run.
//
//      For now, tell the machine where to find the page table.
//----------------------------------------------------------------------

void AddrSpace::RestoreState() 
{
    kernel->machine->pageTable = pageTable;
    kernel->machine->pageTableSize = numPages;
}


//----------------------------------------------------------------------
// AddrSpace::Translate
//  Translate the virtual address in _vaddr_ to a physical address
//  and store the physical address in _paddr_.
//  The flag _isReadWrite_ is false (0) for read-only access; true (1)
//  for read-write access.
//  Return any exceptions caused by the address translation.
//----------------------------------------------------------------------
ExceptionType
AddrSpace::Translate(unsigned int vaddr, unsigned int *paddr, int isReadWrite)
{
    TranslationEntry *pte;
    int               pfn;
    unsigned int      vpn    = vaddr / PageSize;
    unsigned int      offset = vaddr % PageSize;

    if(vpn >= numPages) {
        return AddressErrorException;
    }

    pte = &pageTable[vpn];

    if(isReadWrite && pte->readOnly) {
        return ReadOnlyException;
    }

    pfn = pte->physicalPage;

    // if the pageFrame is too big, there is something really wrong!
    // An invalid translation was loaded into the page table or TLB.
    if (pfn >= NumPhysPages) {
        DEBUG(dbgAddr, "Illegal physical page " << pfn);
        return BusErrorException;
    }

    pte->use = TRUE;          // set the use, dirty bits

    if(isReadWrite)
        pte->dirty = TRUE;

    *paddr = pfn*PageSize + offset;

    ASSERT((*paddr < MemorySize));

    //cerr << " -- AddrSpace::Translate(): vaddr: " << vaddr <<
    //  ", paddr: " << *paddr << "\n";

    return NoException;
}



