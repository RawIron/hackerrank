import math


def generate_input():
    n = int(input())
    for _ in range(n):
        yield int(input())


def is_prime(number):
    if number == 1:
        return False
    if number == 2:
        return True

    if number % 2 == 0:
        return False

    for divisor in range(3, math.floor(math.sqrt(number)+1), 2):
        if number % divisor == 0:
            return False

    return True


if __name__ == "__main__":
    # numbers | is_prime | show
    for number in generate_input():
        if is_prime(number):
            print("Prime")
        else:
            print("Not prime")
