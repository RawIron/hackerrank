#!/bin/python3

def find_pair(cost, money):
    '''
    find the first pair of flavors which will take all of your money
    searches on the set() datatype are supported by a hash table
    find_pair([1,2,4,3,5], 5) == (0,2)
    find_pair([3,4,1,2,5], 5) == (0,3)
    '''
    fast_search_cost = set(cost)
    for i, _ in enumerate(cost):
        if cost[i] >= money:
            break
        remaining = money - cost[i]
        if remaining in fast_search_cost:
            # cannot use index()
            # j = cost.index(remaining, i+1)
            # because of
            # [4,3,2,5,7], 8
            for j, _ in enumerate(cost[i+1:]):
                if cost[i+1+j] == remaining:
                    return (i, i+1+j)

def what_flavors(cost, money):
    (first_flavor, second_flavor) = find_pair(cost, money)
    print(f"{first_flavor+1} {second_flavor+1}")

def parse_input():
    money = int(input())
    _ = int(input())
    cost = list(map(int, input().rstrip().split()))
    return cost, money

def main():
    t = int(input())
    for _ in range(t):
        cost, money = parse_input()
        what_flavors(cost, money)

if __name__ == '__main__':
    main()
