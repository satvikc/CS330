import numpy.random
import math
N = 20
bursts = list(map(math.ceil,numpy.random.exponential(10,N)))
priorities = numpy.random.randint(0,10,N)
interpoisson = numpy.random.poisson(10,N-1)
def running_sum(it):
    s = 0
    yield s
    for i in it:
        s += i
        yield s
arrivals = list(running_sum(interpoisson))

files = ["burst_times.txt", "arrival_times.txt", "priorities.txt"]
times = [bursts , arrivals , priorities]
for (f,t) in zip(files,times):
    f = open(f,'w')
    for i in t:
        f.write(str(i)+"\n")
    f.close()


