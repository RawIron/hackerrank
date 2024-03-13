#!/bin/python3

from collections import Counter


def check_magazine(magazine, note):
    """
    true :  every word in note can be paired
            with the same word in magazine

    note            magazine
                    time
    it              it
    is              is
    time            time    
    is              is
                    over

    >>> check_magazine(["time", "is", "is"], ["time", "is"]) == True
    >>> check_magazine(["time", "is"], ["is", "is"]) == False
    """
    return Counter(note) & Counter(magazine) == Counter(note)


def main():
    _, _ = map(int, input().split())

    magazine = input().rstrip().split()
    note = input().rstrip().split()

    if check_magazine(magazine, note):
        print("Yes")
    else:
        print("No")


if __name__ == '__main__':
    main()
