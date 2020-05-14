#!/bin/python3

import math
import os
import random
import re
import sys
import functools


def factorialReduce(n):
    return functools.reduce(lambda acc, x: x * acc, range(1,n+1), 1)

def factorialRecursive(n):
    def fact(m, acc):
        if m == 0: return acc
        else: return fact(m-1, m*acc)
    return fact(n,1)

factorial = factorialRecursive


def write(number):
    fptr = open(os.environ['OUTPUT_PATH'], 'w')
    fptr.write(str(number) + '\n')
    fptr.close()


if __name__ == '__main__':
    n = int(input())

    result = factorial(n)
    write(result)
