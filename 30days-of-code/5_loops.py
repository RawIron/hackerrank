#!/usr/bin/env python3


def use_side_effect(n):
    # minimal solution
    # function cannot be tested
    for i in range(1,11):
        print(f'{n} x {i} = {n*i}')

LOOPS=10

def use_for(n, m=LOOPS):
    multiplied = []
    for i in range(1,m+1):
        multiplied.append((n,i,n*i))
    return multiplied

def use_zip(n, m=LOOPS):
    return [(a,b,a*b) for (a,b) in zip([n]*m, range(1,m+1))]

def use_map(n, m=LOOPS):
    return map(lambda x: (n,x,n*x), range(1,m+1))

def show(result):
    for r in result:
        print(f'{r[0]} x {r[1]} = {r[2]}')


if __name__ == '__main__':
    n = int(input())

    create_equations = use_for
    show(create_equations(n))
