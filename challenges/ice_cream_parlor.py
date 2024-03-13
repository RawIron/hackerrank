#!/bin/python3

import os
import sys


def find_pair(prices, money):
    '''
    find the first pair of flavors which will take all of your money
    searches on the set() datatype are supported by a hash table

    find_pair([1,2,4,3,5], 5) == (0,2)
    find_pair([3,4,1,2,5], 5) == (0,3)
    '''
    fast_search_cost = set(prices)
    for i, price in enumerate(prices):
        if price >= money:
            continue
        remaining = money - price
        if remaining in fast_search_cost:
            # cannot use index()
            # j = prices.index(remaining, i+1)
            # because of [4,3,2,5,7], 8
            # i=0 is 4 and has 4 as remaining
            # 4 is in the set of prices
            # but it is not in prices[0+1:]
            for j, lookfor in enumerate(prices[i+1:]):
                if lookfor == remaining:
                    return (i, i+1+j)


def what_flavors(cost, money):
    """
    find the two values in the cost list
    for which the sum of them equals the money
    """
    (first_flavor, second_flavor) = find_pair(cost, money)
    return (first_flavor+1, second_flavor+1)


def parse_input():
    money = int(input())
    _ = int(input())
    cost = list(map(int, input().rstrip().split()))
    return cost, money


def show(results):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        for result in results:      
            out.write(' '.join(map(str, result)))
            out.write('\n')


def main():
    results = []

    t = int(input())
    for _ in range(t):
        cost, money = parse_input()
        results.append(what_flavors(cost, money))
    
    show(results)


if __name__ == '__main__':
    """
    don't pollute the module namespace
    """
    main()
