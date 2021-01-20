def mean(sample):
    return sum(sample) / float(len(sample))

def median(sample):
    n = len(sample)
    if n % 2 == 0:
        return (sample[n//2] + sample[(n//2)-1]) / 2.0
    else:
        return sample[n//2]

def mode(sample):
    return max(sample, key=sample.count)


if __name__ == '__main__':
    N = int(input())
    numbers = list(sorted(map(int, input().split())))

    for func in [mean, median]:
        print(f"{func(numbers):.1f}")
    print(mode(numbers))
