#!/bin/python3


def generateSubMatrices(matrix, dim, sub_dim):
    def createSubM(row_idx, column_idx):
        subM = []
        for i in range(row_idx, row_idx+sub_dim):
            subM.append(matrix[i][column_idx:column_idx+sub_dim])
        return subM

    for row in range(dim-sub_dim+1):
        for column in range(dim-sub_dim+1):
            yield createSubM(row, column)


def calcHourGlassSum(matrix,dim):
    sumMiddle = 0
    for i in range(1,dim-1):
        sumMiddle += matrix[i][dim//2]
    return sum(matrix[0]) + sumMiddle + sum(matrix[dim-1])


def generateFixedLines(n):
    # generator objects are not subscriptable !!
    # my_generator_list[4:] will fail at runtime
    for _ in range(n):
        yield map(int, input().rstrip().split())

def readFixedLines(n):
    arr = []
    for _ in range(n):
        arr.append(list(map(int, input().rstrip().split())))
    return arr


if __name__ == '__main__':
    matrix = readFixedLines(6)

    maxSum = ((2 * 3) + ((3-2) * 1)) * (-9) - 1
    for subMatrix in generateSubMatrices(matrix,6,3):
        sumHourGlass = calcHourGlassSum(subMatrix,3)
        maxSum = max(sumHourGlass, maxSum)

    print(maxSum)
