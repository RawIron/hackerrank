class AdvancedArithmetic(object):
    def divisor_sum(self, number):
        raise NotImplementedError


class Calculator(AdvancedArithmetic):
    def divisor_sum(self, number):
        if n == 1:
            return 1

        # an even number is divisible by even and uneven numbers
        # an uneven number is only divisible by uneven numbers
        n_is_even = (n % 2 == 0)

        begin = n // 2
        step = -1
        if not n_is_even:
            step = -2
            # 9 // 2 == 4
            # only uneven numbers can divide 9 so begin at 3
            n_div2_is_even = (n // 2 % 2 == 0)
            if n_div2_is_even:
                begin -= 1

        # every n can be divided by 1 and by itself
        div_sum = n + 1
        for divisor in range(begin, 1, step):
            if (n % divisor) == 0:
                div_sum += divisor

        return div_sum


def solve(number):
    my_calculator = Calculator()
    print("I implemented: " + type(my_calculator).__bases__[0].__name__)
    return my_calculator.divisor_sum(number)


if __name__ == "__main__":
    n = int(input())
    divisor_sum = solve(n)
    print(divisor_sum)
