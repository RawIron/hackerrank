from math import factorial

def binom(p, n, k):
    combinations = factorial(n) / (factorial(k) * factorial(n-k))
    prob = p**k * (1-p)**(n-k)
    return combinations * prob

p_is_boy = 109.0 / 209.0
n = 6

at_least_3 = sum([binom(p_is_boy, n, k) for k in range(3, n+1)])

print(f'{at_least_3:.3f}')
