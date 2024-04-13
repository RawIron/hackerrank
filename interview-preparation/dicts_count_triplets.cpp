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


/*
    store each item with a list of its indices
*/
map<long,vector<int>> positions(const vector<long>& items) {
    map<long,vector<int>> pos{};

    auto idx{0};
    for (const auto& item : items) {
        pos[item].push_back(idx);
        ++idx;
    }

    return pos;
}

/*
*/
vector<pair<int,int>> count_greater(const vector<int>& smaller, const vector<int>& greater) {
    vector<pair<int,int>> greater_counts{};

    auto counter{0};
    auto s_it{ smaller.begin() };
    for (auto g: greater) {
        while (s_it != smaller.end() && *s_it < g) {
            ++counter;
            ++s_it;
        }
        if (counter > 0) {
            greater_counts.push_back( make_pair(g,counter) );
        }
    }

    return greater_counts;
}

vector<pair<int,int>> count_greater(const vector<pair<int,int>>& smaller, const vector<int>& greater) {
    vector<pair<int,int>> greater_counts{};

    auto counter{0};
    auto s_it{ smaller.begin() };
    for (auto g: greater) {
        while (s_it != smaller.end() && s_it->first < g) {
            counter += s_it->second;
            ++s_it;
        }
        if (counter > 0) {
            greater_counts.push_back( make_pair(g,counter) );
        }
    }

    return greater_counts;
}

/*
    find 3 numbers of a geometric series
        (a, a*r, a*r^2)
    where indices of the numbers are in ascending order
        i < j < k
*/
long count_triplets(const vector<long>& numbers, const long r) {
    using triple = array<long, 3>;

    if (r > 31622) {
        // r^2 > 10^9
        // 1 <= numbers <= 10^9
        // last element of the triplet can't be found in the list
        return 0L;
    }

    long total{0};
    const auto s{ positions(numbers) };
    const auto max_number{ (*s.rbegin()).first };

    if (r == 1) {
        for (auto [_, idx_list]: s) {
            const auto len{ idx_list.size() };
            if (len >= 3) {
                total += (len * (len-1) * (len-2)) / 6;
            }
         }
    }

    if (r > 1) {
        for (auto [elem, pos]: s) {
            const auto elem_r_squared = elem * r * r;
            if (elem_r_squared > max_number) {
                continue;
            }
            const triple triplet{ elem, elem * r, elem_r_squared};
            if (s.contains(triplet[1]) && s.contains(triplet[2])) {
                const auto triplet_agg = count_greater(
                                        count_greater(pos, s.at(triplet[1])),
                                        s.at(triplet[2]));
                for (auto&& t: triplet_agg) {
                    total += t.second;
                }
            }
        }
    }

    return total;
}

/*
    tests
*/
void test_count_triplets() {
    {
    const vector<long> have{ 1, 9, 2, 4, 11 };
    const long expected{ 1 };
    assert(count_triplets(have, 2) == expected);
    }
    {
    const vector<long> have{ 1, 1, 4, 2, 4, 7 };
    const long expected{ 2 };
    assert(count_triplets(have, 2) == expected);
    }
    {
    const vector<long> have{ 1, 3, 9, 9, 27, 81 };
    const long expected{ 6 };
    assert(count_triplets(have, 3) == expected);
    }
    {
    const vector<long> have{ 8, 2, 4, 2, 4, 8, 8 };
    const long expected{ 6 };
    assert(count_triplets(have, 2) == expected);
    }
    {
    const vector<long> have{ 1, 1, 1, 1, 3, 3 };
    const long expected{ 4 };
    assert(count_triplets(have, 1) == expected);
    }
    {
    const vector<long> have{ 1, 1, 1, 1, 3, 3 };
    const long expected{ 0 };
    assert(count_triplets(have, 33000) == expected);
    }
    {
    const vector<long> have{ 3, 6, 58, 36 };
    const long expected{ 0 };
    assert(count_triplets(have, 12) == expected);
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