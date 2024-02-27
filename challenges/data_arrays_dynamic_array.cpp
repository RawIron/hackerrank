#include <bits/stdc++.h>

using namespace std;

struct request {
    int type;
    int x;
    int y;
};

void show(const request& r) {
    cout << r.type << " " << r.x << " " << r.y << endl;
}

void show(const vector<request>& requests) {
    for (auto& r : requests) {
        show(r);
    }
}

template <typename T>
void show(const T& items) {
    for (auto& item : items) {
        cout << item << endl;
    }
}

pair<int, vector<request>>
read_input() {
    size_t n{};
    size_t q{};
    cin >> n >> q;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    vector<request> requests{}; 
    int in_type{}, in_x{}, in_y{};

    for (size_t i{0}; i < q; ++i) {
        cin >> in_type >> in_x >> in_y;
        const request r{in_type, in_x, in_y};
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
hash_into_buckets(const int n, const vector<request>& queries) {
    vector<int> result{};
    vector<vector<int>> buckets(n);
    int mask{0};
    
    for (const auto& q: queries) {
        const int hash_value{(q.x ^ mask) % n};
        vector<int>& bucket{buckets[hash_value]};
        if (q.type == 1) {     
            bucket.push_back(q.y);
        }
        if (q.type == 2) {
            mask = bucket[(q.y % bucket.size())];
            result.push_back(mask);
        }
    }
    return result;
}

int main() {
    int n{};
    vector<request> queries{};
    tie(n, queries) = read_input();

    show<vector<int>>(hash_into_buckets(n, queries));

    return EXIT_SUCCESS;
}
