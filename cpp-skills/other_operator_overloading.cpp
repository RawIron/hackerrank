#include <vector>
#include <iostream>
#include <functional>

using namespace std;


typedef vector<vector<int>> Matrix;

/**
 *  fill each element of a (n,m) 2D matrix
 *  with the result of a function call
 */
Matrix matrix_fill(const size_t n, const size_t m,  const function<int(size_t,size_t)>& func) {
    Matrix result;

    for (size_t i=0; i < n; ++i) {
        vector<int> b;
        for (size_t j=0; j < m; ++j) {
            b.push_back( func(i, j) );
        }
        result.push_back(b);
    }

    return result;
}

/**
 *  fold over a 2D matrix
 *  a function which takes an accumulator and an element of the matrix
 *      and returns the accumulator is called for every element
 *  the additional third parameter is set to true
 *      for the last element of a each row
 *
 *  the start value for the fold is set to the default initialization of T
 */
template<typename T>
T matrix_fold(const Matrix& m, const function<T(T, const int, const bool)>& func) {
    T acc{};
    const size_t rows{ m.size() };
    const size_t last_column{  m[0].size()-1 };
    const size_t columns{ m[0].size() };

    for (size_t i=0; i < rows; ++i) {
        for (size_t j=0; j < columns; ++j) {
            if (j == last_column) {
                acc = func(acc, m[i][j], true);
            }
            else {
                acc = func(acc, m[i][j], false);
            }
        }
    }

    return acc;
}


Matrix operator+(const Matrix& lhs, const Matrix& rhs) {
    const size_t n{ lhs.size() };
    const size_t m{ lhs[0].size() };

    return
    matrix_fill(n, m, [lhs, rhs](size_t i, size_t j) { return lhs[i][j] + rhs[i][j]; } );
}


Matrix read(const size_t n, const size_t m) {
    return
    matrix_fill(n, m, [](size_t i, size_t j) { int num; cin >> num; return num; } );
}

/**
 *  print the 2D matrix one row per line
 *  do a fold but no calculation is done
 *      ignore the accumulator argument
 */
void show(const Matrix& m) {
    auto lambda = [](int _, const int elem, const bool last_col) {
        if (! last_col) { cout << elem << " "; }
        else { cout << elem << endl; }
        return 0;
        };

    matrix_fold<int>(m, lambda);
    return;
}


int main () {
   int cases;
   cin >> cases;

   for (int k=0; k<cases; k++) {
        int n, m;
        cin >> n >> m;

        Matrix x{ read(n, m) };
        Matrix y{ read(n, m) };

        Matrix result{ x+y };
        show(result);
   }

   return EXIT_SUCCESS;
}
