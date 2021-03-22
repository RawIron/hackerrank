import sys

t = int(input())

for _ in range(0,t):
    n = int(input())
    lengths = list(map(int, input().split()))
    
    last = sys.maxsize
    l = 0
    r = len(lengths)-1
    
    while (l <= r):        
        if lengths[l] >= lengths[r]:
            picked = lengths[l]
            l += 1
        else:
            picked = lengths[r]
            r -= 1
        
        if picked > last:
            break
        last = picked
        
    if l <= r:
        print("No")
    else:
        print("Yes")
