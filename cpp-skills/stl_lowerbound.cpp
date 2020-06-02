#include <iostream>
#include <vector>
#include <tuple>
#include <algorithm>

using namespace std;


template<typename T>
vector<T> readMany() {
    size_t n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    vector<T> in_list{};
    for (size_t i{0}; i<n; ++i) {
        T element{};
        cin >> element;

        in_list.push_back(element);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return in_list;
}

template<typename T>
pair<bool, size_t> solve(const vector<T> haystack, const T needle) {
    typename vector<T>::const_iterator pos{ lower_bound(haystack.begin(), haystack.end(), needle) };
    const bool found{ (*pos == needle) };
    return make_pair(found, pos-haystack.begin());
}


int main() {
    const vector<int> haystack{ readMany<int>() };
    const vector<int> needles{ readMany<int>() };

    for (const auto& needle : needles) {
        bool found{};
        size_t pos{};
        tie(found, pos) = solve<int>(haystack, needle);
        if (found) {
            cout << "Yes " << pos+1 << endl;
        }
        else {
            cout << "No " << pos+1 << endl;
        }
    }

    return 0;
}
