#include <bits/stdc++.h>

using namespace std;


int factorial(const int n) {
    function<int(int,int)> fact;
    fact = [&fact](const int m, const int acc) {
        if (m == 0) { return acc; }
        else { return fact(m-1, m*acc); }
    };
    return fact(n, 1);
}

template <typename T>
void showSingle(const T value) {
    ofstream fout(getenv("OUTPUT_PATH"));
    fout << value << "\n";
    fout.close();  
}

template <typename T>
T readSingle() {
    T n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    return n;
}

#if __cplusplus > 201703L
void solve() {
    // use ranges
    readInput() | factorial() | show();
}
#endif

int main()
{
    int n = readSingle<int>();
    auto result = factorial(n);
    showSingle(result);

    return 0;
}
