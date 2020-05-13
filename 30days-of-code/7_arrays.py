#!/bin/python3

import math
import os
import random
import re
import sys

def read_input():
    _ = input()
    return input().rstrip().split()

if __name__ == '__main__':
    numbers = read_input()
    print(' '.join(reversed(numbers)))
