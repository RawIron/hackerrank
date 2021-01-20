from math import sqrt


def mean(sample):
    return sum(sample) / float(len(sample))


def standard_deviation(sample):
    mu = mean(sample)
    return sqrt(sum([(x - mu)**2 for x in sample]) / len(sample))


if __name__ == '__main__':
    N = int(input())
    numbers = list(map(int, input().split()))

    print(f"{standard_deviation(numbers):.1f}")
