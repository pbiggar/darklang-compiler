# Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
import sys
rounds, token = sys.argv[1:]
middle = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" * 2
short = "ab" + token + "cd"
long = "prefix:" + token + ":" + middle + ":suffix"
short_cases = [short, "".join(["a", "b", token, "cd"]), short+"x",
               "xb"+token+"cd", "ab"+token+"ce"]
long_cases = [long, "".join(["pre", "fix:", token, ":", middle, ":suffix"]),
              long+"!", "xrefix:"+token+":"+middle+":suffix",
              "prefix:"+token+":"+middle+":suffiy"]
total = 0
for _ in range(int(rounds)):
    for i, other in enumerate(short_cases):
        if short == other: total += 1 << i
    for i, other in enumerate(long_cases):
        if long == other: total += 32 << i
print(total)
