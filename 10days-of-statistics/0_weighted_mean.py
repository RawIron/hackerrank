def weighted_mean(sample, weights):
    return sum([sample[i] * weights[i] for i in range(len(sample))]) / sum(weights)

if __name__ == "__main__":
    N = int(input())
    x = list(map(int, input().split()))
    w = list(map(int, input().split()))

    print(f"{weighted_mean(x, w):.1f}")
