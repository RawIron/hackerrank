#!/bin/python3

import os
import sys
from collections import Counter

def find_max_adjacent(numbers):
    '''
    find longest subset with any two elements are neighbors
    for [3,4,4,5,5,1] the subsets are [3,4,4], [4,4,5,5], [5,5], [1]
    picking [3,4,4,5,5,1] == 4
    '''
    max_len = 0
    counts = Counter(numbers)
    for n, count in counts.items():
        max_len = max(max_len, count + counts[n+1])
    return max_len

def parse_input():
    _ = int(input().strip())
    return map(int, input().rstrip().split())

def write_answer(result):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')

def main():
    numbers = parse_input()
    write_answer(find_max_adjacent(numbers))

if __name__ == '__main__':
    main()
