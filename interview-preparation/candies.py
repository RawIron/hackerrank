#!/bin/python3

import os
import sys


def show(result):
    fileno = os.environ.get('OUTPUT_PATH', sys.stdout.fileno())
    with open(fileno, 'w') as out:
        out.write(str(result) + '\n')


def read_input():
    n = int(input())

    scores = []
    for _ in range(n):
        score = int(input())
        scores.append(score)

    return scores


def candies(scores):
    '''
    use the minimum count of candies such that
    of any neighbors the one with a higher score
    receives more candies

        0 0 0 1 0 0 0 1 2 3     down streak

        0 1 2 2 0 1 2 2 2 2     up streak
                    .
                   / \
            .     .   .
           / \   /     \        scores
          .   . .       .
         /               \
        .                 .
        1 2 3 1 1 2 4 3 2 1     candies
    '''
    candies_total = 1
    previous_score = scores[0]
    up_streak = 0
    down_streak = 0

    for score in scores[1:]:
        if score > previous_score and down_streak > 0:
            #  .
            #   \
            #    .   .
            #     \./
            #  6 3 2 3
            if down_streak > up_streak:
                candies_total += down_streak - up_streak
            down_streak = 0
            up_streak = 1
            candies_total += 2
        elif score > previous_score and down_streak == 0:
            #      .
            #     /
            #    .
            #   /
            #  2 4 8
            up_streak += 1
            candies_total += 1 + up_streak
        elif score == previous_score and down_streak > 0:
            #  .
            #   \
            #    .
            #     \. .
            #  6 3 2 2
            if down_streak > up_streak:
                candies_total += down_streak - up_streak
            down_streak = 0
            up_streak = 0
            candies_total += 1
        elif score == previous_score and down_streak == 0:
            #    . .       . . .
            #   /
            #  2 4 4       4 4 4
            up_streak = 0
            candies_total += 1
        else:
            #  .          .
            #   \        / \
            #    .      .   .
            #  6 3        6 3
            down_streak += 1
            candies_total += down_streak
        previous_score = score

    if down_streak > 0:
        if down_streak > up_streak:
            candies_total += down_streak - up_streak

    return candies_total


def main():
    scores = read_input()
    result = candies(scores)
    show(result)


if __name__ == '__main__':
    '''
    a clean global namespace
    '''
    main()