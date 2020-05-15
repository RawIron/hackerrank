#include <bits/stdc++.h>

using namespace std;

using matrix2d = vector<vector<int>>;


matrix2d createSubMatrix(const matrix2d matrix, const size_t rowIdx, const size_t columnIdx, const size_t dim) {
    matrix2d subMatrix{};
    
    for (size_t i{rowIdx}; i<rowIdx+dim; ++i) {
        vector<int> row{};
        for (size_t j{columnIdx}; j<columnIdx+dim; ++j) {
            row.push_back(matrix[i][j]);
        }
        subMatrix.push_back(row);
    }

    return subMatrix;
}

vector<matrix2d> createSubMatrices(const matrix2d matrix, const size_t dim, const size_t sub_dim) {
    vector<matrix2d> subMatrices{};

    for (size_t i{0}; i<dim-sub_dim+1 ; ++i) {
        for (size_t j{0}; j<dim-sub_dim+1 ; ++j) {
            subMatrices.push_back(createSubMatrix(matrix, i, j, sub_dim));
        }
    }

    return subMatrices;
}


int calcSumHourGlass(const matrix2d matrix, const size_t dim) {
    int sum{0};
    int init_value{0};

    sum += std::accumulate(matrix[0].begin(), matrix[0].end(), init_value);
    sum += matrix[1][1];
    sum += std::accumulate(matrix[dim-1].begin(), matrix[dim-1].end(), init_value);

    return sum;
}

/**
 * stringstream: map(split(' '), take(sstream, N, '\n'))
 */
template <typename T>
vector<vector<T>> readFixedLinesMultiValues(const size_t DIM) {
    vector<vector<T>> lines(DIM);
    
    for (size_t i{0}; i < DIM; ++i) {
        lines[i].resize(DIM);
        for (size_t j{0}; j < DIM; ++j) {
            cin >> lines[i][j];
        }
        cin.ignore(numeric_limits<streamsize>::max(), '\n');
    }

    return lines;
}

void printMatrix(const matrix2d matrix) {
    for (auto row : matrix) {
        for (auto value : row) {
            cout << value << " ";
        }
        cout << endl;
    }
}


int main() {
    constexpr int number_lines = 6;

    function <matrix2d(int)> readInput = readFixedLinesMultiValues<int>;
    matrix2d matrix = readInput(number_lines);

    int sum{};
    int maxSum{-64};
    for (matrix2d subMatrix : createSubMatrices(matrix, 6, 3)) {
        sum = calcSumHourGlass(subMatrix, 3);
        maxSum = max(sum, maxSum);
    }
    cout << maxSum << endl;

    return 0;
}
