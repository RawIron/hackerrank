def solve(numbers, m, like, dislike):
    like = dict(zip(like, [1]*m))
    dislike = dict(zip(dislike, [-1]*m))
    weights = like.copy()
    weights.update(dislike)

    total = 0
    for number in numbers:
        if number in weights:
            total += weights[number]

    return total


def main():
    n, m = map(int, input().split())
    numbers = map(int, input().split())
    like = map(int, input().split())
    dislike = map(int, input().split())

    total = solve(numbers, m, like, dislike)

    print(total)


if __name__ == '__main__':
    main()
