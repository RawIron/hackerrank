class Difference:
    def __init__(self, numbers):
        self.__elements = numbers
        self.maximum = 0

    def compute(self):
        self.maximum = max(self.__elements) - min(self.__elements)


_ = input()
numbers = [int(e) for e in input().split(' ')]

difference = Difference(numbers)
difference.compute()

print(difference.maximum)
