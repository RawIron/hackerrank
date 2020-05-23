#include <iostream>
#include <vector>
#include <string>

using namespace std;


template<typename T>
void printArray(const vector<T> items) {
    for(auto item : items) {
        cout << item << endl;
    }
}


template<typename T>
vector<T> readFixedMany() {
    size_t n{};
    cin >> n;

    vector<T> in_vector(n);
    for (size_t i{0}; i < n; ++i) {
        T value;
        cin >> value;
        in_vector[i] = value;
    }
    return in_vector;
}


int main() {
    vector<int> int_vector{ readFixedMany<int>() };
    vector<string> string_vector{ readFixedMany<string>() };

    printArray<int>(int_vector);
    printArray<string>(string_vector);

    return 0;
}
