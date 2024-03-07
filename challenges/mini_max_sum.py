import sys


def mini_max_sum(numbers):
    is_sorted = sorted(numbers)
    return sum(is_sorted[:4]), sum(is_sorted[-4:])

def test_simple_sum():
    assert mini_max_sum([3, 4, 6, 2, 2]) == (11, 15)


def main():
    raw = list(map(int, input().strip().split()))
    mini, maxi = mini_max_sum(raw)
    print(f"{mini} {maxi}")

if __name__ == "__main__":
    test_simple_sum()
    main()