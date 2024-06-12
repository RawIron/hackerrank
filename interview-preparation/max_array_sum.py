#!/bin/python3

import os
import sys


def show(result):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')


def search_max(memo, pos_sequence, begin):
    if (len(pos_sequence) < 3):
        return pos_sequence[0]

    plus2_path = 0
    plus3_path = 0

    if (begin+2 not in memo):
        plus2_path = search_max(memo, pos_sequence[2:], begin+2)
        memo[begin+2] = plus2_path
    else:
        plus2_path = memo[begin+2]
    
    if (len(pos_sequence) > 3):
        if (begin+3 not in memo):
            plus3_path = search_max(memo, pos_sequence[3:], begin+3)
            memo[begin+3] = plus3_path
        else:
            plus3_path = memo[begin+3]

    max_path = max(plus2_path, plus3_path) + pos_sequence[0]
    memo[begin] = max_path

    # if (len(pos_sequence) <= 3):
    #     print(pos_sequence[0], (pos_sequence[2], plus2_path))
    # else:
    #     print(pos_sequence[0], (pos_sequence[2], plus2_path), (pos_sequence[3], plus3_path))

    return max_path


def test_search_max():
    tests = [
        ((dict(), [3, 6, 5, 12, 1, 2, 24, 7], 0), 39),
        ((dict(), [2,3], 0), 2)
    ]

    for test in tests:
        (have, expected) = test
        (memo, seq, begin) = have
        print(search_max(memo, seq, begin))


def partition(numbers):
    partitions = list()
    indices = [i for i, x in enumerate(numbers) if x < 0]
    indices.append(len(numbers))

    begin = 0
    for end in indices:
        part = numbers[begin:end]
        if part:
            partitions.append(part)
        begin = end+1

    return partitions


def test_partition():
    tests = [
        ([3, 6, -5, -12, 1, 2, 24, -7], [[3, 6], [1, 2, 24]]),
        ([-2, 1 , 3, -4, 5], [[1, 3], [5]])
    ]

    for test in tests:
        (have, expected) = test
        print(partition(have))


'''
    build any subset from elements which are not neighbors
    find the subset with the largest sum
    
    [3,4,5,-1,-2,4,6,-5,1] == [3,5,6,1]
    [3,7,4,6,5] == []
    [6,3,8,9,5,1,2,12,2,28] == []
    
    any negative number partitions the search
     => only look at sequences of positive numbers
'''
def max_sum_subset(numbers):
    parts = partition(numbers)
    max_sum = 0

    for part in parts:
        if (len(part) == 1):
            max_sum += part[0]
        elif (len(part) == 2):
            max_sum += max(part[0], part[1])
        else:
            memo = dict()     
            max_uneven = search_max(memo, part[::], 0)
            max_even = search_max(memo, part[1::], 1)   
            max_sum += max(max_uneven, max_even)
    
    return max_sum


def test_max_sum_subset():
    tests = [
        ([3, 6, -5, -12, 1, 2, 24, -7], 31),
        ([-2, 1 , 3, -4, 5], 8)
    ]

    for test in tests:
        (have, expected) = test
        print(max_sum_subset(have)) 


def run_tests():
    test_search_max()
    test_partition()
    test_max_sum_subset()


def solve():
    _ = int(input())
    numbers = map(int, input().split())
    max_sum = max_sum_subset(list(numbers))
    show(max_sum)


if __name__ == '__main__':
    '''
        keep global namespace clean
    '''
    run_tests()
    # solve()
