#!/bin/python3

from collections import Counter


def check_magazine(magazine, note):
    if Counter(note) & Counter(magazine) == Counter(note):
        print("Yes")
    else:
        print("No")


if __name__ == '__main__':
    mn = input().split()
    m = int(mn[0])
    n = int(mn[1])

    magazine = input().rstrip().split()
    note = input().rstrip().split()

    check_magazine(magazine, note)
