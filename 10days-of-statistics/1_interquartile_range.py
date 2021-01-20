from itertools import chain


def median(sample):
    n = len(sample)
    if n % 2 == 0:
        return (sample[n//2] + sample[(n//2)-1]) / 2.0
    else:
        return sample[n//2]


def split_at_median(sample):
    n = len(sample)
    if n % 2 == 0:
        return (sample[0:n//2], sample[n//2:])
    else:
        return (sample[0:n//2], sample[(n//2)+1:])


if __name__ == '__main__':
    N = int(input())
    numbers = list(map(int, input().split()))
    frequencies = list(map(int, input().split()))

    sorted_numbers = sorted(chain(*[[numbers[i]]*frequencies[i] for i in range(len(numbers))]))

    lower_half, upper_half = split_at_median(sorted_numbers)
    q1 = median(lower_half)
    q3 = median(upper_half)

    print(f"{q3-q1:.1f}")
