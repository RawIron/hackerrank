#include <iostream>
#include <vector>
#include <string>

using namespace std;


template<typename T>
void show(const vector<T>& items) {
    for(const auto& item : items) {
        cout << item << endl;
    }
}

template<typename T>
vector<T> read_many() {
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
    const auto int_vector{ read_many<int>() };
    const auto string_vector{ read_many<string>() };

    show(int_vector);
    show(string_vector);

    return EXIT_SUCCESS;
}
