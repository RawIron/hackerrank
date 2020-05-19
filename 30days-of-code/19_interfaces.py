import math

class AdvancedArithmetic(object):
    def divisor_sum(self, number):
        raise NotImplementedError


class Calculator(AdvancedArithmetic):
    def divisor_sum(self, n):
        if n == 1:
            return 1

        # an even number is divisible by even and uneven numbers
        # an uneven number is only divisible by uneven numbers
        n_is_even = (n % 2 == 0)
        if n_is_even:
            begin = 2
            step = 1
        else:
            begin = 3
            step = 2

        # sqrt(6) = 2.45
        # ceil(2.45) = 3
        # only search up to 2
        # thus range upper bound in loop should be 3
        sqrt_n = math.sqrt(n)
        ceil_sqrt_n = math.ceil(sqrt_n)

        # every n can be divided by 1 and by itself
        div_sum = n + 1
        # square root is a divisor of n
        # add it only once
        if sqrt_n == ceil_sqrt_n and n % sqrt_n == 0:
            # sqrt(9) = 3
            # ceil_sqrt(3) = 3
            div_sum += sqrt_n

        # devisors are communatative
        #    6 // 2 == 3
        # => 6 // 3 == 2
        # 2,3 are divisors
        for divisor in range(begin, ceil_sqrt_n, step):
            if (n % divisor) == 0:
                div_sum += divisor
                div_sum += n // divisor

        return div_sum

def solve(number):
    my_calculator = Calculator()
    print("I implemented: " + type(my_calculator).__bases__[0].__name__)
    return my_calculator.divisor_sum(number)


if __name__ == "__main__":
    n = int(input())
    divisor_sum = solve(n)
    print(divisor_sum)
