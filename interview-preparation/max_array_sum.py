#!/bin/python3

import os
import sys


def show(result):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')


def search(memo, pos_sequence, begin):
    '''
    at any point in the search for the subset with the max sum
    there are two possible moves
        jump over the neighbor (plus 2)
        skip the element next to the neighbor (plus 3)

    jump ahead any further would skip a positive number
    that could have been added to the sum
    thus the resulting sum of a subset containing such a
    long jump will not be maximal
        *        *                      (plus 4)
        *   *    *                      (plus 2, plus 2)
        3 6 5 12 1 : 3 + 1 < 3 + 5 + 1

    a jump any shorter (plus 1) would land on the neighbor

      3 6 5 12 1 2 24 7

              3
            /   \
          5      12
         /  \    / \
        1   2   2   24
      /  \  |   |
     24  7  7   7
    '''
    if len(pos_sequence) < 3:
        return pos_sequence[0]

    search_sums = list()

    for jump in [2, 3]:
        if len(pos_sequence) <= jump:
            break

        jump_sum = 0

        if begin + jump not in memo:
            jump_sum = search(memo, pos_sequence[jump:], begin + jump)
            memo[begin + jump] = jump_sum
        else:
            jump_sum = memo[begin + jump]

        search_sums.append(jump_sum)

    max_sum = max(search_sums) + pos_sequence[0]
    memo[begin] = max_sum

    DEBUG = False
    if DEBUG:
        if len(pos_sequence) <= 3:
            print(pos_sequence[0], (pos_sequence[2], search_sums[0]))
        else:
            print(pos_sequence[0], (pos_sequence[2], search_sums[0]), (pos_sequence[3], search_sums[1]))

    return max_sum


def test_search():
    tests = [
        ((dict(), [3, 6, 5, 12, 1, 2, 24, 7], 0), 39),
        ((dict(), [2,3], 0), 2)
    ]

    return [search(memo, seq, begin) == expected for ((memo, seq, begin), expected) in tests]


def search_max(pos_sequence):
    '''
    from a sequence of positive numbers find the subset with the largest sum
    a subset can only contain elements which are not neighbors in the sequence

    as no subset can contain neighbors the search starts either with the first
    or with the second element
    '''
    if len(pos_sequence) == 1:
        return pos_sequence[0]
    if len(pos_sequence) == 2:
        return max(pos_sequence[0], pos_sequence[1])

    memo = dict()  
    start_fst_max = search(memo, pos_sequence[::], 0)
    start_snd_max = search(memo, pos_sequence[1::], 1)

    return max(start_fst_max, start_snd_max)


def partition(numbers):
    '''
    from a list of numbers extract the sequences of positive numbers

    partition([1,2,-3,-4,5,-6]) == [[1,2], [5]]
    '''
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

    return [partition(have) == expected for (have, expected) in tests]


def max_sum_subset(numbers):
    '''
    build any subset from elements which are not neighbors
    and find the subset with the largest sum

    max_sum_subset([3,4,5,-1,-2,4,6,-5,1]) == sum([3,5,6,1]) == 15

    any negative number partitions the search
    => only look at sequences of positive numbers
    '''
    parts = partition(numbers)
    max_sum = 0

    for part in parts:   
        max_sum += search_max(part)
    
    return max_sum


def test_max_sum_subset():
    tests = [
        ([3, 6, -5, -12, 1, 2, 24, -7], 31),
        ([-2, 1 , 3, -4, 5], 8),
        ([3,4,5,-1,-2,4,6,-5,1], 15),
        ([3,7,4,6,5], 13),
        ([6,3,8,9,5,1,2,12,2,28], 59)
    ]

    return [max_sum_subset(have) == expected for (have, expected) in tests]


def run_tests():
    print(test_search())
    print(test_partition())
    print(test_max_sum_subset())


def solve():
    _ = int(input())
    numbers = map(int, input().split())
    max_sum = max_sum_subset(list(numbers))
    show(max_sum)


if __name__ == '__main__':
    '''
    a clean global namespace
    '''
    run_tests()
    # solve()
