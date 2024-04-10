#include <bits/stdc++.h>

using namespace std;

template <typename T>
vector<T> read_many(const int n) {
    assert( n > 0 );
    vector<T> items{};

    copy_if(
        istream_iterator<T>(cin),
        istream_iterator<T>(),
        back_inserter(items),
        [count=n] (T) mutable { return count && count--; }
    );
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return items;
}

template <typename T>
pair<T,T> read_two() {
    T fst{}, snd{};
    cin >> fst;
    cin >> snd;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    return make_pair(fst, snd);
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


template<typename T>
map<T,int> frequencies(const vector<T>& items) {
    map<T,int> freq{};

    for (const T& item : items) {
        ++freq[item];
    }

    return freq;
}


long count_triplets(const vector<long>& numbers, long r) {
    using triple = array<long, 3>;

    long total{0};
    const auto s{ frequencies<long>(numbers) };

    for (auto [elem, count]: s) {
        const triple triplet{ elem, elem * r, elem * r * r };
        if (s.contains(triplet[1]) && s.contains(triplet[2])) {
            total += s.at(triplet[0]) * s.at(triplet[1]) * s.at(triplet[2]);
        }
    }

    return total;
}

void test_count_triplets() {
    {
    const vector<long> have{ 1, 9, 2, 4, 11 };
    const long expected{ 1 };
    assert(count_triplets(have, 2) == expected);
    }
    {
    const vector<long> have{ 1, 1, 4, 2, 4, 7 };
    const long expected{ 4 };
    assert(count_triplets(have, 2) == expected);
    }
    {
    const vector<long> have{ 1, 3, 9, 9, 27, 81 };
    const long expected{ 6 };
    assert(count_triplets(have, 3) == expected);
    }
}


void solve() {
    auto [n, r] = read_two<int>();
    vector<long> numbers{ read_many<long>(n) };
    show( count_triplets(numbers, r) );
}


int main() {
    test_count_triplets();
    // solve();
    return EXIT_SUCCESS;
}
