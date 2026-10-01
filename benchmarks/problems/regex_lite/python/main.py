# Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import re
import sys
blocks,runs = map(int,sys.argv[1:])
text = b"darklang darkxxlang compiler42 compiler ab ab7 nope DARKlang compilerx\n"*blocks
pattern = re.compile(rb"dark[a-z]*lang|compiler[0-9]+|ab[0-9]?")
total = 0
for _ in range(runs):
    count = checksum = 0
    for position in range(len(text)):
        if pattern.match(text,position):
            checksum = (checksum+(position+1)*(count+3)) % 1_000_000_007
            count += 1
    total = (total+count*1_000_003+checksum) % 1_000_000_007
print(total)
