class Difference:
    def __init__(self, numbers):
        self.__elements = numbers
        self.maximum = 0

    def compute(self):
        self.maximum = max(self.__elements) - min(self.__elements)


def read_input():
    _ = input()
    return [int(e) for e in input().split(' ')]


if __name__ == "__main__":
    numbers = read_input()

    difference = Difference(numbers)
    difference.compute()

    print(difference.maximum)
