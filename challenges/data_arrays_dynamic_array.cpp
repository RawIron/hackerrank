#include <bits/stdc++.h>

using namespace std;

enum class qtype {
    store = 1,
    retrieve
};

ostream& operator << (ostream& os, const qtype& obj) {
   os << static_cast<underlying_type<qtype>::type>(obj);
   return os;
}
struct query {
    qtype type;
    int x;
    int y;
};

void show(const query& q) {
    cout << q.type << " " << q.x << " " << q.y << endl;
}

void show(const vector<query>& queries) {
    for (auto& q : queries) {
        show(q);
    }
}

template <typename T>
void show(const T& items) {
    for (auto& item : items) {
        cout << item << endl;
    }
}

pair<int, vector<query>>
read_input() {
    size_t n{};
    size_t q{};
    cin >> n >> q;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    vector<query> requests(q); 
    int in_type{}, in_x{}, in_y{};
    for (auto _ : views::iota(static_cast<size_t>(0), q)) {
        cin >> in_type >> in_x >> in_y;
        const query r{static_cast<qtype>(in_type), in_x, in_y};
        requests.push_back(r);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return make_pair(n, requests);
}

/**
    run queries which
        store values into buckets
        retrieve values from buckets
 */
vector<int>
hash_into_buckets(const int n, const vector<query>& queries) {
    vector<int> result{};

    using enum qtype;
    vector<vector<int>> buckets(n);
    int mask{0};
    for (auto& q: queries) {
        const int hash_value{(q.x ^ mask) % n};
        auto& bucket{buckets[hash_value]};
        if (q.type == store) {     
            bucket.push_back(q.y);
        }
        if (q.type == retrieve) {
            mask = bucket[(q.y % bucket.size())];
            result.push_back(mask);
        }
    }

    return result;
}

int main() {
    auto [n, queries] = read_input();

    show(hash_into_buckets(n, queries));

    return EXIT_SUCCESS;
}
