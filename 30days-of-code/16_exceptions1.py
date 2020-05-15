#!/bin/python3

import sys


word = input().strip()

try:
    number = int(word)
    print(number)
except:
    print("Bad String")
