#!/bin/python3


def generate_pairs():
    t = int(input())
    for _ in range(t):
        yield map(int, input().split())


def solve_bruteforce(n, k):
    high = 0
    for a in range(1, n+1):
        for b in range(a+1, n+1):
            bitwise = a & b
            if bitwise < k:
                high = max(bitwise, high)
    return high


def solve_bitlogic(n, k):
    '''
    n = 9
    k-1 = 4 = 0100
    b   = 5 = 0101
    => a = 4 and b = 5

    n = 9
    k-1 = 7 = 0111
    a   = 6 = 0110
    => a = (k-2) = 6 and b = 7

    n = 16
    k-1 = 7 = 00111
    b  = 15 = 01111
    => a = 7 and b = 15
    '''
    high = 0

    for a in range(k-1, 0, -1):
        shift_it = a
        min_b = 0

        # copy trailing 1's
        while (shift_it & 1) > 0:
            min_b = (min_b << 1) | 1
            shift_it = shift_it >> 1

        # a is   010011
        # flip   000111
        min_b = (min_b << 1) | 1

        # copy remaining bits
        # min_b  010111
        min_b = min_b | a

        if min_b <= n:
            high = a
            break

    return high


if __name__ == '__main__':
    # this is the global namespace !!
    read_input = generate_pairs
    solve = solve_bitlogic

    for (set_size, upper_bound) in read_input():
        max_bitwise = solve(set_size, upper_bound)
        print(max_bitwise)
