#include <iostream>

using namespace std;


template<typename T>
class AddElements {
public:
    explicit AddElements(const T& value) : value_{value} {};
    
    template<typename NUM,
             typename = enable_if_t< is_arithmetic_v<NUM> >>
    NUM add(const NUM rhs) {
        return value_ + rhs;
    }

    template<typename STR,
             typename = enable_if_t<
                        is_convertible_v<STR, string> >>
    STR concatenate(const STR& rhs) {
        return value_ + rhs;
    }

private:
    T value_;
};


template<typename T> pair<T, T> read() {
    T first{};
    T second{};
    cin >> first >> second;
    return {first, second};
}


int main () {
    // there are 500_000 tests
    // each has 3 reads with cin
    //      and 1 write with cout
    // around 80% of the total runtime
    ios::sync_with_stdio(false);    // detach C++ streams from stdio
                                    // eliminates the double‑buffering between C and C++ runtimes
    cin.tie(nullptr);               // stop implicit cout flushes
                                    // removes the hidden fflush(stdout) that happens on every cin extraction

    int n{};
    cin >> n;

    for (int i{0}; i < n; ++i) {
        string type{};
        cin >> type;

        if (type=="float") {
            auto [elem1, elem2] = read<double>();

            AddElements<double> myfloat(elem1);
            cout << myfloat.add(elem2) << endl;
        }
        else if (type == "int") {
            auto [elem1, elem2] = read<int>();

            AddElements<int> myint(elem1);
            cout << myint.add(elem2) << endl;
        }
        else if (type == "string") {
            auto [elem1, elem2] = read<string>();

            AddElements<string> mystring(elem1);
            cout << mystring.concatenate(elem2) << endl;
        }
    }

    return EXIT_SUCCESS;  
}
