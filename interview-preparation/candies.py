#!/bin/python3

import math
import os
import random
import re
import sys


def candies(n, scores):
    '''
    '''
    candies_total = 0
    previous_score = 0
    latest_peak = 0
    down_streak = 0

    for score in scores:
        if score > previous_score and down_streak > 0:
            if down_streak >= latest_peak:
                candies_total += down_streak - latest_peak + 1
            down_streak = 0
            latest_peak = 2
            candies_total += latest_peak
        elif score > previous_score and down_streak == 0:
            latest_peak += 1
            candies_total += latest_peak
        elif score == previous_score and down_streak > 0:
            if down_streak >= latest_peak:
                candies_total += down_streak - latest_peak + 1
            down_streak = 0
            latest_peak = 1
            candies_total += 1
        elif score == previous_score and down_streak == 0:
            latest_peak = 1
            candies_total += 1
        else:
            down_streak += 1
            candies_total += down_streak
        previous_score = score

    if down_streak > 0:
        if down_streak >= latest_peak:
            candies_total += down_streak - latest_peak + 1

    return candies_total


def show(result):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')


def main():
    n = int(input())

    scores = []
    for _ in range(n):
        score = int(input())
        scores.append(score)

    result = candies(n, scores)

    show(result)


if __name__ == '__main__':
    main()