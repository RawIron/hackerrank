#!/bin/python3

import os
import sys

def count_repeats(word, n):
    '''
    count how many times the character 'a' occurs
    in a repeated string
    repeat the string up to length of n
    
    'aba' repeats for length 7 are 'abaabaa'
    count_repeats('aba', 7) == 5
    '''
    word_len = len(word)
    words = n // word_len
    reminder = n % word_len
    return (words * word.count('a') + word[:reminder].count('a'))

def main():
    s = input()
    n = int(input().strip())

    result = count_repeats(s, n)
    
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')

if __name__ == '__main__':
    main()
