import numpy.random
import math
N = 20
q = 2
def cpubursts(q):
    return list(map(math.ceil,numpy.random.exponential(10,q)))

def iobursts(q):
    return numpy.random.randint(10,20,q)

priorities = numpy.random.randint(0,10,N)
interpoisson = numpy.random.poisson(10,N-1)
def running_sum(it):
    s = 0
    yield s
    for i in it:
        s += i
        yield s
arrivals = list(running_sum(interpoisson))

files = ["arrival_times.txt", "priorities.txt"]
times = [arrivals , priorities]
for (f,t) in zip(files,times):
    f = open(f,'w')
    for i in t:
        f.write(str(i)+"\n")
    f.close()

f = open("cpu_bursts.txt",'w')
for i in range(N):
    cpuburstList = cpubursts(q);
    for j in cpuburstList:
        f.write(str(j)+" ")
    f.write("\n");
f.close()

f = open("io_bursts.txt",'w')
for i in range(N):
    ioburstsList = iobursts(q-1);
    for j in ioburstsList:
        f.write(str(j)+" ")
    f.write("\n");
f.close()


