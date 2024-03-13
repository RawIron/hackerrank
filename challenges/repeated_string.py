#!/bin/python3

import os
import sys

def count_repeats(word, n):
    '''
    count how many times the character 'a' occurs
    in a repeated string
    repeat the string up to length of n
    
    'aba' repeated to a length of 7 is 'abaabaa'
    count_repeats('aba', 7) == 5
    '''
    word_len = len(word)
    words = n // word_len
    reminder = n % word_len
    return (words * word.count('a') + word[:reminder].count('a'))


def show(result):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')


def main():
    s = input()
    n = int(input().strip())

    result = count_repeats(s, n)

    show(result)


if __name__ == '__main__':
    main()
