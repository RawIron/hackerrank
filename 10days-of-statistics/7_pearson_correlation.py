from statistics import mean, pstdev

def pearson(X,Y,n):
    mean_x = mean(X)
    mean_y = mean(Y)
    return sum((x-mean_x)*(y-mean_y) for x,y in zip(X,Y)) / (n * pstdev(X) * pstdev(Y))


if __name__ == '__main__':
    n = int(input())
    X = list(map(float, input().strip().split()))
    Y = list(map(float, input().strip().split()))
    
    print(f'{pearson(X,Y,n):.3f}')
