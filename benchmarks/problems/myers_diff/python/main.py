# Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import sys
MOD = 1_000_000_007

def diff(left, right):
    def snake(x,y):
        while x < len(left) and y < len(right) and left[x] == right[y]:
            x += 1; y += 1
        return x,y
    x,y = snake(0,0)
    work = (x+1)*(y+3)
    if x == len(left) and y == len(right): return 0,work
    previous = {0:x}
    for d in range(1,len(left)+len(right)+1):
        frontier = {}; reached = False
        for k in range(-d,d+1,2):
            if k == -d or (k != d and previous[k-1] < previous[k+1]):
                start = previous[k+1]
            else: start = previous[k-1]+1
            x,y = snake(start,start-k)
            work = (work+(x+1)*(y+3)+(k+d+1)*17) % MOD
            frontier[k] = x
            reached |= x >= len(left) and y >= len(right)
        if reached: return d,work
        previous = frontier
    raise ValueError("unreachable diff")
blocks, insertions, runs = map(int,sys.argv[1:])
unit = b"darklang compiler benchmark: persistent values and recursive paths.\n"
prefix, suffix = unit*blocks, unit*(blocks+1)
left, right = prefix+suffix, prefix+b"<changed-block>"*insertions+suffix
total = 0
for _ in range(runs):
    distance,work = diff(left,right)
    total = (total+distance*1_000_003+work) % MOD
print(total)
