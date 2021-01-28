import numpy as np
import numpy.linalg as la


class LinearRegression():
    def __init__(self):
        self.b = np.array([])
    
    def fit(self, X, y):
        self.b = la.inv(X.T.dot(X)).dot(X.T).dot(y)
        return self
    
    def predict(self, x):
        return self.b.dot(x.T)


def read_input():
    m, n = map(int, input().strip().split())

    xfit_in = []
    yfit_in = []
    for _ in range(n):
        line = list(map(float, input().strip().split()))
        xfit_in.append([1] + line[0:m])
        yfit_in.append(line[m])
    X_fit = np.array(xfit_in)
    y_fit = np.array(yfit_in)

    q = int(input().strip())
    xpred_in = []
    for _ in range(q):
        xpred_in.append([1] + list(map(float, input().strip().split())))
    X_pred = np.array(xpred_in)

    return X_fit, y_fit, X_pred


if __name__ == '__main__':
    X_fit, y_fit, X_pred = read_input()

    lm = LinearRegression().fit(X_fit, y_fit)
    y_pred = lm.predict(X_pred)

    for pred in y_pred:
        print(f'{pred:.2f}')
