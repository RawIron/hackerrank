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
    numbers = list(sorted(map(int, input().split())))

    lower_half, upper_half = split_at_median(numbers)

    print(f"{median(lower_half):.1f}")
    print(f"{median(numbers):.1f}")
    print(f"{median(upper_half):.1f}")
