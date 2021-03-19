from collections import defaultdict
from collections import OrderedDict

class stats():
    counter = 0
    first = 10^5

words = defaultdict(stats)

n = int(input())
for i in range(0,n):
    key = input()
    words[key].counter += 1
    words[key].first = min(words[key].first, i)

by_first = OrderedDict(sorted(words.items(), key=lambda t: t[1].first))

print(len(words.keys()))
for key in by_first.keys():
    print(words[key].counter, end=' ')
