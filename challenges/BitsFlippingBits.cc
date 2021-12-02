#include <bits/stdc++.h>

using namespace std;

uint32_t flipBits(const uint32_t n) {
    return ~n;
}


template<typename T>
void printArray(const vector<T> items) {
    ofstream fout(getenv("OUTPUT_PATH"));
    for(auto item : items) {
        fout << item << endl;
    }
    fout.close();
}

template<typename T>
vector<T> readFixedMany() {
    size_t n{};
    cin >> n;

    vector<T> in_vector(n);
    for (size_t i{0}; i < n; ++i) {
        T value{};
        cin >> value;
        in_vector[i] = value;
    }
    return in_vector;
}

int main() {
    vector<uint32_t> numbers{ readFixedMany<uint32_t>() };

    vector<uint32_t> results{};
    for (auto number : numbers) {
        results.push_back( flipBits(number) );
    }

    printArray(results);

    return 0;
}
