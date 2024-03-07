#!/bin/python3


def generate_sub_matrices(matrix, dim, sub_dim):
    def create_subm(row_idx, column_idx):
        subm = []
        for i in range(row_idx, row_idx+sub_dim):
            subm.append(matrix[i][column_idx:column_idx+sub_dim])
        return subm

    for row in range(dim-sub_dim+1):
        for column in range(dim-sub_dim+1):
            yield create_subm(row, column)


def calc_hourglass_sum(matrix,dim):
    sum_middle = 0
    for i in range(1,dim-1):
        sum_middle += matrix[i][dim//2]
    return sum(matrix[0]) + sum_middle + sum(matrix[dim-1])


def solve(matrix):
    max_sum = ((2 * 3) + ((3-2) * 1)) * (-9) - 1
    for sub_matrix in generate_sub_matrices(matrix,6,3):
        sum_hourglass = calc_hourglass_sum(sub_matrix,3)
        max_sum = max(sum_hourglass, max_sum)
    return max_sum


def generate_fixed_lines(n):
    ''' generator objects are not subscriptable !!
        my_generator_list[4:] will fail at runtime
    '''
    for _ in range(n):
        yield map(int, input().rstrip().split())


def read_fixed_lines(n):
    arr = []
    for _ in range(n):
        arr.append(list(map(int, input().rstrip().split())))
    return arr


if __name__ == '__main__':
    solution = solve( read_fixed_lines(6) )
    print(solution)
