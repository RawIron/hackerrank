def solve(lengths):
    last = 2**31
    l = 0
    r = len(lengths)-1

    while l <= r:
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
        return False
    else:
        return True

def read():
    n = int(input())
    return list(map(int, input().split()))

def main():
    tests = int(input())
    for _ in range(0, tests):
        lengths = read()

        can_be_solved = solve(lengths)
        if can_be_solved:
            print("Yes")
        else:
            print("No")

main()
