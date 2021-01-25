try:
    from scipy.stats import norm
except ModuleNotFoundError:
    # Python Version >= 3.8
    try:
        from statistics import NormalDist
        norm = NormalDist
    except (ModuleNotFoundError, ImportError):
        from math import erf, sqrt
        class norm(object):
            def __init__(self, mu=0.0, sigma=1.0):
                self.mu = mu
                self.sigma = sigma
            def cdf(self, x):
                # Cumulative distribution function for the standard normal distribution
                return (1.0 + erf( (x-self.mu) / (self.sigma * sqrt(2.0)) )) / 2.0


if __name__ == '__main__':
    mu, sigma = map(float, input().strip().split())
    smaller_than = float(input().strip())
    left, right = map(float, input().strip().split())

    phi = norm(mu, sigma).cdf

    print(f"{phi(smaller_than):.3f}")
    print(f"{phi(right)-phi(left):.3f}")
