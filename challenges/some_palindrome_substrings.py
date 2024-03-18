#!/bin/python3

import os
import sys
import random


#
#   a digression on
#   building strings containing palindromes
#   
#   this needs plenty of polishing
#

class builder():
    '''
    Palindromes
    prm(ch,n,n) = Type 1: repeats of same character with middle (left == right)
    prm(ch,n,m) = Type 1: repeats of same character with middle (left < right)
    prm(ch,n,m) = Type 1: repeats of same character with middle (left > right)
    pr(ch,n)    = Type 2: repeats of same character
    fill(ch,n)  = Filler

    prm(a,n,m) = pr(a,n) + fill(b,1) + pr(a,m)
    '''
    alphabet = 'abcdefghijklmnopqrstuvwxyz'

    def __init__(self):
        self.value = ''
        self.chain = lambda current, new: current + new
        self.registers = {}
        self.take = 0

    def concat(self):
        self.chain = lambda current, new: current + new
        return self

    def overlap(self, n):
        def chain_with_overlap(current, new):
            if len(current) < n or len(new) < n:
                raise ValueError
            if current[-n:] != new[:n]:
                pass
                #raise ValueError
            return current + new[n:]
        self.chain = chain_with_overlap
        self.take = self.taken
        return self

    def copy_to(self, name):
        if name in self.registers:
            raise ValueError
        self.registers[name] = self.value
        return self

    def copy_from(self, name):
        self.value += self.registers[name]
        return self

    def copy(self, n=1):
        self.temp = self.value
        for _ in range(n):
            self.value = self.chain(self.value, self.temp)
        return self

    def prm(self, n, m):
        left = self._min_draw_identicals(n)
        middle = self._min_draw_uniques(1)
        right = self._min_draw_given(left[0], m)
        self.value = self.chain(self.value, left + middle + right)
        return self

    def pr(self, n):
        self.value = self.chain(self.value, self._min_draw_identicals(n))
        return self

    def fill(self, n):
        self.value = self.chain(self.value, self._min_draw_uniques(n))
        return self

    def result(self):
        return self.value

    def _min_draw_given(self, letter, repeats=1):
        self.taken = builder.alphabet.index(letter)
        return letter * repeats

    def _min_draw_identicals(self, repeats):
        if repeats == 1:
            return self._min_draw_uniques(1)
        drawn = builder.alphabet[self.take] * repeats
        self.taken = builder.alphabet[self.take]
        if self.take == 0:
            self.take += 1
        else:
            self.take = 0
        return ''.join(drawn)

    def _min_draw_uniques(self, n=1):
        if self.take > 2:
            self.take = 0
        drawn = builder.alphabet[self.take:self.take+n]
        self.taken = self.take + n-1
        self.take += n
        return ''.join(drawn)

    @staticmethod
    def _random_draw(n=1):
        drawn = set()
        while len(drawn) != n:
            drawn.add(builder.alphabet[random.randint(0, 25)])
        return ''.join(drawn)


def test_string_create():
    tests = [
        (builder().fill(1).concat().fill(1).concat().fill(1).concat().fill(1).value, 'abca'),
        (builder().fill(1).concat().pr(2).concat().fill(1).value, 'abba'),
        (builder().prm(1, 1).concat().fill(1).value, 'abac'),
        (builder().fill(2).value, 'ab'), 
        (builder().fill(1).concat().prm(1,1).concat().fill(1).result(), 'abcba'),
        (builder().pr(4).value, 'aaaa'),
        (builder().pr(4).concat().fill(1).value, 'aaaab'),
        (builder().prm(2, 3).value, 'aabaaa'),
        (builder().fill(2).concat().pr(4).value, 'abcccc'),
        (builder().fill(1).concat().pr(2).concat().pr(4).concat().fill(1).value, 'abbaaaab'),
        (builder().prm(1, 1).overlap(1).prm(1, 1).value, 'ababa'),
        (builder().prm(1, 1).overlap(1).copy().value, 'ababa'),
        (builder().pr(3).overlap(1).copy(2).value, 'aaaaaaa'),
        ]

    for result, expected in tests:
        if result != expected:
            print(f'test failed: expected {expected} found {result}')


def run_tests():
    tests = [('cab', 3, builder().fill(3)),
             ('cabcabca', 8, builder().fill(8)),
             # simple pr
             ('aab', 4, builder().pr(2).concat().fill(1)),
             ('bcaab', 6, builder().fill(2).concat().pr(2).concat().fill(1)),
             ('bcaa', 5, builder().fill(2).concat().pr(2)),
             ('a', 1, builder().pr(1)),
             ('aa', 3, builder().pr(2)),
             ('aaa', 6, builder().pr(3)),
             ('aaaa', 10, builder().pr(4)),
             ('aabc', 5, builder().pr(2).concat().fill(2)),
             ('aaabbbc', 13, builder().pr(3).concat().pr(3).concat().fill(1)),
             ('aaabbaaabb', 18, builder().pr(3).concat().pr(2).copy()),
             ('aaabbbaaabbb', 24, builder().pr(3).concat().pr(3).copy()),
             # simple prm
             ('aba', 4, builder().prm(1,1)),
             ('caba', 5, builder().fill(1).concat().prm(1,1)),
             ('cabac', 6, builder().fill(1).concat().prm(1,1).concat().fill(1)),
             ('aaabaa', 12, builder().prm(3,2)),
             ('aabaaa', 12, builder().prm(2,3)),
             ('aaba', 6, builder().prm(2,1)),
             ('aabaaac', 13, builder().prm(2,3).concat().fill(1)),
             # prm overlaps
             ('cdbaabaaba', 15, builder().fill(3).concat().prm(2,2).overlap(2).prm(2,1)),
             ('ababab', 10, builder().fill(2).concat().copy(2)),
             ('caaabaa', 13, builder().fill(1).concat().prm(3,2)),
             ('abbcccca', 15, builder().fill(1).concat().pr(2).concat().pr(4).concat().fill(1)),
             ('cabbbabbc', 15, []),
             ('caababbbc', 15, []),
             ('cabbbabbcbbacbabbccccaacbaacbacaacbcbac', 62, []),
             ('baccacbaaaaacbaccaabccaccbccaccbccbbabcca', 71, []),
             ('cbccccccacaaaabbacbcbaaaaccccbbacbaaabbcabcaabccbcbabbbabc', 111, []),
            ]

    for word, expected, _ in tests:
        result = count_some_palindromes(len(word), word)
        if result != expected:
            print(f'test for {len(word)},{word} failed: expected {expected} found {result}')


#
#   the solution to the challenge
#
#

def count_some_palindromes(n, word):
    '''
    count all substrings which are a palindrome

    a palindrome can only be one of two things:
    1. repeating the same letter like 'aaa'
    2. repeating the same letter on both sides
       of a single separating letter, like 'aacaa'

    it is not required to
    3. list the palindrome itself
    4. list the length of the palindrome

    == same letter repeated ==

    compare the last 2 letters and update the length of the repeat

    == uneven length palindromes ==
    
    for a letter sequence like
        aaaabaaac -> [aba, aabaa, aaabaaa]
    the counter should be increased by 3
    
    because of restriction 2 increase the count by 1 for
        abcdefghgfedcba <=> ghg
    reminder: [fghgf, efghgfe, ..] are __not__ counted

    it is sufficient to look at the last 3 letters read from the string
    (repeat_letter, last_letter, letter)
    '''
    count = n
    has_middle = False  # true: there is a left repeat and the right repeat is
                        #       in progress 
    left_repeat = 0     # holds an immutable copy of the last right_repeat                        
    right_repeat = 1    # incremented when a letter is repeated

    DEBUG = False
    repeat_letter = ''
    last_letter = ''

    if DEBUG: print(count)
    for ix, letter in enumerate(word):
        if DEBUG: print(word[:ix+1])
        if letter != last_letter:
            if right_repeat > 1:
                # aab
                count += (right_repeat * (right_repeat-1)) // 2
                if DEBUG: print(count)
                if has_middle:
                    count += max(0, min(right_repeat-1, left_repeat-1))
                    has_middle = False
                if DEBUG: print(count)
                left_repeat = right_repeat
                right_repeat = 1
            else:
                if letter == repeat_letter:
                    # aba
                    count += 1
                    if has_middle:
                        # bbaba
                        left_repeat = 0
                        has_middle = False
                    else:
                        if left_repeat > 1:
                            # aaaba
                            has_middle = True
                    if DEBUG: print(count)
                else:
                    # abc
                    left_repeat = 0
                    has_middle = False
        else:
            # baa
            right_repeat += 1
        repeat_letter = last_letter
        last_letter = letter

    count += (right_repeat * (right_repeat-1)) // 2
    if has_middle:
        count += max(0, min(right_repeat-1, left_repeat-1))

    return count


def show(result):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')


def solve():
    n = int(input())
    s = input()
    result = count_some_palindromes(n, s)
    show(result)


if __name__ == '__main__':
    run_tests()
    solve()
    #test_string_create()
