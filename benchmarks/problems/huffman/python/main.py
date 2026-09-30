# Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
from collections import Counter
import heapq
import sys
MOD = 1_000_000_007

def generate(n,seed):
    data = []
    for _ in range(n):
        seed = (seed*1103515245+12345) % 2147483648
        r = seed % 1000
        symbol = next((i for i,t in enumerate((300,480,610,710,790,850,900,940))
                       if r < t), 8+seed%24)
        data.append(symbol)
    return data

def codec(data):
    queue = [(weight,symbol,symbol) for symbol,weight in Counter(data).items()]
    heapq.heapify(queue)
    while len(queue) > 1:
        w1,s1,t1 = heapq.heappop(queue)
        w2,s2,t2 = heapq.heappop(queue)
        heapq.heappush(queue,(w1+w2,min(s1,s2),(t1,t2)))
    tree = queue[0][2]
    codes = {}
    def visit(node,bits,length):
        if isinstance(node,int): codes[node] = bits,max(1,length)
        else:
            visit(node[0],bits*2,length+1)
            visit(node[1],bits*2+1,length+1)
    visit(tree,0,0)
    return tree,codes

def checksum(values): return sum(v*(i+1) for i,v in enumerate(values)) % MOD
n,seed,runs = map(int,sys.argv[1:])
data = generate(n,seed)
tree,codes = codec(data)
total = 0
for _ in range(runs):
    encoded = [bits >> i & 1 for symbol in data for bits,length in [codes[symbol]]
               for i in range(length-1,-1,-1)]
    decoded = []
    current = tree
    for bit in encoded:
        if isinstance(tree,int): decoded.append(tree); continue
        current = current[bit]
        if isinstance(current,int): decoded.append(current); current = tree
    if decoded != data: raise ValueError("Huffman roundtrip failed")
    total = (total+len(encoded)*17+checksum(encoded)+checksum(decoded)) % MOD
print(total)
