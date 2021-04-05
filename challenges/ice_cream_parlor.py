#!/bin/python3

def find_pair(cost, money):
    # find the first pair of flavors which will take all of your money
    # searches on the set() datatype are supported by a hash table
    fast_search_cost = set(cost)
    for i, _ in enumerate(cost):
        if cost[i] >= money:
            break
        remaining = money - cost[i]
        if remaining in fast_search_cost:
            for j in range(i+1, len(cost)):
                if cost[j] == remaining:
                    return (i, j)

def what_flavors(cost, money):
    (first_flavor, second_flavor) = find_pair(cost, money)
    print(f"{first_flavor+1} {second_flavor+1}")

def main():
    t = int(input())
    for _ in range(t):
        money = int(input())
        n = int(input())
        cost = list(map(int, input().rstrip().split()))

        what_flavors(cost, money)

if __name__ == '__main__':
    main()
