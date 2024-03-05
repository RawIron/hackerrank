#include <bits/stdc++.h>

using namespace std;

template <typename T>
void show(const T value) {
    const char* const out_path{ getenv("OUTPUT_PATH") };
    if (out_path != nullptr) {
        ofstream fout(out_path);
        fout << value << endl;
    }
    else {
        cout << value << endl;
    }
}

template<typename T>
vector<T> read_many() {
    size_t n{};
    cin >> n;

    const size_t zero{0};
    vector<T> in_vector(n);
    for (auto i : views::iota(zero,n)) {
        T value{};
        cin >> value;
        in_vector[i] = value;
    }
    return in_vector;
}


uint32_t flip_bits(const uint32_t n) {
    return ~n;
}

int main() {
    auto numbers{ read_many<uint32_t>() };

    for (auto flipped : numbers | views::transform(flip_bits)) {
        show(flipped);
    }

    return EXIT_SUCCESS;
}
