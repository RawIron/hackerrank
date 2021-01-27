def spearman_rank(X, Y, n):
    def create_r(V):
        ranks_v = dict((number, ix) for ix, number in enumerate(sorted(V)))
        return [ranks_v[number] for number in V]

    r_x = create_r(X)
    r_y = create_r(Y)
    d_squared = [(rx - ry)**2 for rx, ry in zip(r_x, r_y)]

    return 1 - ((6 * sum(d_squared)) / (n * (n**2 - 1)))


if __name__ == '__main__':
    n = int(input())
    X = list(map(float, input().strip().split()))
    Y = list(map(float, input().strip().split()))

    print(f'{spearman_rank(X,Y,n):.3f}')
