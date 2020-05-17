class Calculator:
    def __init__(self):
        pass

    def power(self, number, exponent):
        if number < 0 or exponent < 0:
            raise ValueError("n and p should be non-negative")
        return pow(number, exponent)


def read_number_pairs():
    number_lines = int(input())
    for _ in range(number_lines):
        yield map(int, input().split())


def solve(base, exponent):
    my_calculator = Calculator()
    try:
        ans = my_calculator.power(base, exponent)
        print(ans)
    except ValueError as error:
        print(error)


if __name__ == "__main__":
    for n, p in read_number_pairs():
        solve(n, p)
