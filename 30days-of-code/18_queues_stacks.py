import sys
from collections import deque


class Solution:
    def __init__(self):
        self.word_stack = []
        self.word_queue = deque([])

    def push(self, letter):
        self.word_stack.append(letter)
        return self

    def pop(self):
        return self.word_stack.pop()

    def enqueue(self, letter):
        self.word_queue.append(letter)
        return self

    def dequeue(self):
        return self.word_queue.popleft()


def solve(word):
    solution = Solution()

    for letter in word:
        solution.push(letter)
        solution.enqueue(letter)

    is_palindrome = True
    for _ in range(len(word) // 2):
        if solution.pop() != solution.dequeue():
            is_palindrome = False
            break

    return is_palindrome


def read_input():
    return input()


if __name__ == "__main__":
    in_word = read_input()

    if solve(in_word):
        print("The word, " + in_word + ", is a palindrome.")
    else:
        print("The word, " + in_word + ", is not a palindrome.")
