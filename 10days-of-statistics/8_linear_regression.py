class LinearRegression(object):
    def __init__(self):
        self.a = 0.0
        self.b = 0.0

    def fit(self, X, y):
        n=len(y)
        sX = sum(X)
        sy = sum(y)
        sX2 = sum(x*x for x in X)
        sXy = sum(x*y for x, y in zip(X, y))
        self.b = ((n * sXy) - (sX * sy)) / ((n * sX2) - (sX ** 2))
        self.a = (sy / n) - (self.b * (sX / n))
        return self

    def predict(self, x):
        return self.a + (self.b * x)


X = []
y = []
for _ in range(5):
    x_in, y_in = map(int, input().strip().split())
    X.append(x_in)
    y.append(y_in)

lm = LinearRegression().fit(X, y)
pred_stats_score = lm.predict(80)

print(f'{pred_stats_score:.3f}')
