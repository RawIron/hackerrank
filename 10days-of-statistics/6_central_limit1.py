from math import erf, sqrt

class norm(object):
    def __init__(self, mu=0.0, sigma=1.0):
        self.mu = mu
        self.sigma = sigma
    def cdf(self, x):
        # Cumulative distribution function for the standard normal distribution
        return (1.0 + erf( (x-self.mu) / (self.sigma * sqrt(2.0)) )) / 2.0


n = 49
mu = 205
sigma = 15

phi = norm(n * mu, sqrt(n) * sigma).cdf

print(f'{phi(9800):.4f}')
