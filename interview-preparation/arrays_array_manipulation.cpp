#include <bits/stdc++.h>

using namespace std;

struct query {
    size_t begin;
    size_t end;
    long addend;
};

void show(const query& r) {
    cout << r.begin << " " << r.end << " " << r.addend << endl;
}

void show(const vector<query>& queries) {
    for (auto& r : queries) {
        show(r);
    }
}

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

pair<long, vector<query>> read_input() {
    size_t n{};
    size_t q{};
    cin >> n >> q;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    size_t in_begin{}, in_end{};
    long in_addend{};
    vector<query> queries{};

    for (size_t i{0}; i < q; ++i) {
        cin >> in_begin >> in_end >> in_addend;
        const query r{in_begin, in_end, in_addend};
        queries.push_back(r);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return make_pair(n, queries);
}

/*

 */
long array_manipulation(const long n, const vector<query>& queries) {
    vector<long> data(n, 0);

    for (const auto& q : queries) {
        for (auto i=q.begin-1; i < q.end; ++i) {
            data[i] += q.addend;
        }
    }
    return *max_element(begin(data), end(data));
}

int main() {
    auto [n, queries] = read_input();

    long result = array_manipulation(n, queries);
    show(result);

    return EXIT_SUCCESS;
}
