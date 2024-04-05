#include <bits/stdc++.h>

using namespace std;

struct query {
    size_t begin;
    size_t end;
    long addend;

    bool operator==(const query& rhs) const noexcept {
        return tie(begin, end, addend) == tie(rhs.begin, rhs.end, rhs.addend);
    }

    bool operator<(const query& rhs) const noexcept {
        return tie(begin, end) < tie(rhs.begin, rhs.end);
    }
};

void show(const query& r) {
    cout << r.begin << " " << r.end << " " << r.addend << endl;
}

void show(const vector<query>& queries) {
    for (auto& r : queries) {
        show(r);
    }
}

void show(const set<query>& queries) {
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
        query r{in_begin, in_end, in_addend};
        queries.push_back(r);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return make_pair(n, queries);
}


/*
    two points

    id
    ab  cd

    swallow
    abcd            a++d
*/
set<query> overlap_ab_cd(const query ab, const  query cd) {
    set<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(a == b && c == d);

    if (a == c) {
        // swallow
        const query ad{a, d, ab.addend + cd.addend};
        pieces.insert(ad);
    }

    if (a != c) {
        // id
        ;;
    }

    return pieces;
}


/*
    one point and one interval with length > 0

    id
    ab  c__d
    c__d  ab

    swallow
    c_ab_d          c_a  a+b  b_d
    cab__d          ca+b  b__d
    c__abd          c__a  ab+d
*/
set<query> overlap_ab(const query ab, const  query cd) {
    set<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(c < d);
    assert(a == b);

    if (a < c) {
        // id
        ;;
    }
    if (d < a) {
        // id
        ;;
    }

    if (c < a && a < d) {
        const query ca{c, a-1, cd.addend};
        const query aa{a, a, ab.addend + cd.addend};
        const query ad{a+1, d, cd.addend};

        pieces.insert(ca);
        pieces.insert(aa);
        pieces.insert(ad);
    }

    if (c == a) {
        const query ac{a, a, ab.addend + cd.addend};
        const query bd{a+1, d, cd.addend};

        pieces.insert(ac);
        pieces.insert(bd);
    }

    if (a == d) {
        const query ca{c, a-1, cd.addend};
        const query bb{a, a, ab.addend + cd.addend};

        pieces.insert(ca);
        pieces.insert(bb);
    }

    return pieces;
}


/*
    two intervals with length > 0
    (!) expects
        ab.begin == cd.begin

    swallow
    ac---bd         a++d
    ac__d--b        ac++d  d--b
    ac--b__d        ac++b  b__d
*/
set<query> overlap_ac(const query ab, const  query cd) {
    set<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(a < b);
    assert(c < d);
    assert(a == c);

    if (b == d) {
        const query ad{a, d, ab.addend + cd.addend};
        pieces.insert(ad);
    }

    if (b > d) {
        const query ad{a, d, ab.addend + cd.addend};
        const query db{d+1, b, ab.addend};

        pieces.insert(ad);
        pieces.insert(db);
    }

    if (b < d) {
        const query cb{a, b, ab.addend + cd.addend};
        const query bd{b+1, d, cd.addend};

        pieces.insert(cb);
        pieces.insert(bd);
    }

    return pieces;
}

/*
    two intervals with length > 0
    (!) expects
        ab.end == cd.end

    swallow
    ac---bd         a++d
    c__a-bd        c__a  a++d
    a--c_bd        a--c  c++d
*/
set<query> overlap_bd(const query ab, const  query cd) {
    set<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(a < b);
    assert(c < d);
    assert(b == d);

    if (a == c) {
        const query ad{a, d, ab.addend + cd.addend};
        pieces.insert(ad);
    }

    if (a > c) {
        const query ca{c, a-1, cd.addend};
        const query ad{a, d, ab.addend + cd.addend};

        pieces.insert(ca);
        pieces.insert(ad);
    }

    if (a < c) {
        const query ac{a, c-1, ab.addend};
        const query cb{c, b, ab.addend + cd.addend};

        pieces.insert(ac);
        pieces.insert(cb);
    }

    return pieces;
}

/*
    two intervals with length > 0
    (!) expects
        ab.begin < cd.begin
        ab.end != cd.end

    id
    a--b   c__d

    swallow
    a--c___d--b     a--c  c++d  d--b

    overlap
    a--c_b__d       a--c  c++b  b__d
    a--bc__d        a--b  b+c  c__d
*/
set<query> overlap_a_c(const query ab, const  query cd) {
    set<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(a < b);
    assert(c < d);
    assert(a < c);

    if (b < c) {
        // id
        ;;
    }

    if (c < b && d < b) {
        // swallow
        const query ac{a, c-1, ab.addend};
        const query cd_{c, d, ab.addend + cd.addend};
        const query db{d+1, b, ab.addend};

        pieces.insert(ac);
        pieces.insert(cd_);
        pieces.insert(db);
    }

    if (c < b && b < d) {
        // overlap
        const query ac{a, c-1, ab.addend};
        const query cb{c, b, cd.addend + ab.addend};
        const query bd{b+1, d, cd.addend};

        pieces.insert(ac);
        pieces.insert(cb);
        pieces.insert(bd);
    }

    if (b == c) {
        // overlap
        const query ac{a, b-1, ab.addend};
        const query bc{b, b, cd.addend + ab.addend};
        const query bd{b+1, d, cd.addend};

        pieces.insert(ac);
        pieces.insert(bc);
        pieces.insert(bd);
    }

    return pieces;
}

set<query> overlap(query ab, query cd) {
    if (ab.begin == ab.end && cd.begin == cd.end) { return overlap_ab_cd(ab, cd); }

    if (ab.begin == ab.end && cd.begin < cd.end) { return overlap_ab(ab, cd); }
    if (ab.begin < ab.end && cd.begin == cd.end) { return overlap_ab(cd, ab); }

    if (ab.begin < ab.end && cd.begin < cd.end
        && ab.begin == cd.begin) { return overlap_ac(ab, cd); }
    if (ab.begin < ab.end && cd.begin < cd.end
        && ab.end == cd.end) { return overlap_bd(ab, cd); }

    if (ab.begin < ab.end && cd.begin < cd.end
        && ab.begin < cd.begin) { return overlap_a_c(ab, cd); }
    if (ab.begin < ab.end && cd.begin < cd.end
        && cd.begin < ab.begin) { return overlap_a_c(cd, ab); }

    return set<query> {};
}


/*
    write result of test to ostream
*/
void test_open(const string& name) {
    cout << name << '\n';
}

void test_close() {
    cout << endl;
}

void test_out(bool pass) {
    if (pass) {
        cout << ".";
    }
    else {
        cout << "F";
    }
    cout << flush;
}

void test_overlap_ab_cd() {
    test_open("overlap_ab_cd");
    {
    //  *
    //      *
    const query ab{2, 2, 5};
    const query cd{7, 7, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{};
    test_out(result == expected);
    }
    {
    //  *
    //  *
    const query ab{2, 2, 5};
    const query cd{2, 2, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {2,2,6} };
    test_out(result == expected);
    }
    test_close();
}

void test_overlap_ab() {
    test_open("overlap_ab");
    {
    //  *
    //     +---+
    const query ab{2, 2, 5};
    const query cd{4, 7, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{};
    test_out(result == expected);
    }
    {
    //         *
    //  +----+
    const query ab{8, 8, 5};
    const query cd{4, 7, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{};
    test_out(result == expected);
    }
    {
    //    *
    //  +-----+
    const query ab{2, 2, 5};
    const query cd{1, 4, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {1,1,1}, {2,2,6}, {3,4,1} };
    test_out(result == expected);
    }
    {
    //  *
    //  +---+
    const query ab{1, 1, 5};
    const query cd{1, 4, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {1,1,6}, {2,4,1} };
    test_out(result == expected);
    }
    {
    //       *
    //  +----+
    const query ab{4, 4, 5};
    const query cd{1, 4, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {1,3,1}, {4,4,6} };
    test_out(result == expected);
    }
    test_close();
}

void test_overlap_ac() {
    test_open("overlap_ac");
    {
    //  +---+
    //  +-----+
    const query ab{2, 4, 5};
    const query cd{2, 7, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {2,4,6}, {5,7,1} };
    test_out(result == expected);
    }
    {
    //  +-----+
    //  +---+
    const query ab{2, 7, 5};
    const query cd{2, 4, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {2,4,6}, {5,7,5} };
    test_out(result == expected);
    }
    {
    //  +---+
    //  +---+
    const query ab{2, 4, 5};
    const query cd{2, 4, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {2,4,6} };
    test_out(result == expected);
    }
    test_close();
}

void test_overlap_a_c() {
    test_open("overlap_a_c");
    {
    //  +--+
    //        +---+
    const query ab{2, 4, 5};
    const query cd{5, 7, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{};
    test_out(result == expected);
    }
    {
    //  +-----+
    //      +-----+
    const query ab{2, 7, 5};
    const query cd{5, 8, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {2,4,5}, {5,7,6}, {8,8,1} };
    test_out(result == expected);
    }
    {
    //  +------+
    //     +---+
    const query ab{1, 5, 5};
    const query cd{3, 5, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {1,2,5}, {3,5,6} };
    test_out(result == expected);
    }
    {
    //  +--------+
    //     +---+
    const query ab{2, 8, 5};
    const query cd{5, 7, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {2,4,5}, {5,7,6}, {8,8,5} };
    test_out(result == expected);
    }
    {
    //  +---+
    //      +---+
    const query ab{2, 5, 5};
    const query cd{5, 7, 1};
    auto result = overlap(ab, cd);
    const set<query> expected{ {2,4,5}, {5,5,6}, {6,7,1} };
    test_out(result == expected);
    }
    test_close();
}

void test_overlap() {
    test_overlap_ac();
    test_overlap_ab_cd();
    test_overlap_ab();
    test_overlap_a_c();
}

void test_set() {
    {
    const query value{ 2,4,6 };
    set<query> intervals{ {2,4,5}, {5,7,6}, {8,8,1} };
    const query expected{ 2,4,5 };
    test_out(*intervals.lower_bound(value) == expected);
    }
    {
    const query value{ 2,4,6 };
    set<query> intervals{ {2,3,5}, {5,7,6}, {8,8,1} };
    const query expected{ 5,7,6 };
    test_out(*intervals.lower_bound(value) == expected);
    }
    {
    const query value{ 9,11,3 };
    set<query> intervals{ {2,4,5}, {5,7,6}, {8,8,1} };
    set<query>::iterator expected{ intervals.end() };
    test_out(intervals.lower_bound(value) == expected);
    }
    {
    const query value{ 1,2,1 };
    set<query> intervals{ {2,4,5}, {5,7,6}, {8,8,1} };
    const query expected{ 2,4,5 };
    test_out(*intervals.lower_bound(value) == expected);
    }
    {
    const query value{ 2,4,6 };
    set<query> intervals{};
    set<query>::iterator expected{ intervals.end() };
    test_out(intervals.lower_bound(value) == expected);       
    }
    {
    set<query> intervals{ {2,4,6} };
    set<query>::iterator expected{ intervals.end() };
    test_out(ranges::next(intervals.begin(), intervals.end()) == expected);       
    }
}

long array_manipulation(const long _n, const vector<query>& queries);
void test_manipulation() {
    {
    //               *
    //   +---+
    //         +--+
    const vector<query> queries{ {8,8,1}, {2,4,5}, {5,7,6} };
    long expected{ 6L };
    test_out(array_manipulation(0L, queries) == expected);       
    }
    {
    //       *
    //   *
    //         *
    const vector<query> queries{ {5,5,1}, {2,2,5}, {8,8,7} };
    long expected{ 7L };
    test_out(array_manipulation(0L, queries) == expected);       
    }
    {
    //   *
    //   +---+
    //          *
    const vector<query> queries{ {2,2,5}, {2,5,3}, {8,8,7} };
    long expected{ 8L };
    test_out(array_manipulation(0L, queries) == expected);       
    }
    {
    //   +--+
    //   +---+
    //   +------+
    const vector<query> queries{ {2,4,5}, {2,5,8}, {2,8,7} };
    long expected{ 20L };
    test_out(array_manipulation(0L, queries) == expected);       
    }
    {
    //   +--+
    //     +----+
    //   +----+
    const vector<query> queries{ {2,3,5}, {4,8,7}, {2,5,3} };
    long expected{ 10L };
    test_out(array_manipulation(0L, queries) == expected);       
    }
    {
    //    +---+
    //      +---+
    //   +--------+
    const vector<query> queries{ {2,4,5}, {3,5,3}, {1,8,7} };
    long expected{ 15L };
    test_out(array_manipulation(0L, queries) == expected);       
    }
    {
    //    +-----+
    //      +--+
    //   +--------+
    //         +----+
    const vector<query> queries{ {2,6,8}, {3,5,7}, {1,8,1}, {5,9,15} };
    long expected{ 31L };
    test_out(array_manipulation(0L, queries) == expected);       
    }
}


/*

 */
long array_manipulation_naive(const long n, const vector<query>& queries) {
    vector<long> data(n, 0);

    for (const auto& q : queries) {
        for (auto i=q.begin-1; i < q.end; ++i) {
            data[i] += q.addend;
        }
    }
    return *max_element(begin(data), end(data));
}


/*

*/
long array_manipulation(const long _n, const vector<query>& queries) {
    if (queries.empty()) { return 0L; }
    if (queries.size() == 1) { return queries[0].addend; }

    set<query> non_overlapping{ *queries.begin() };

    for (auto it = next(queries.begin()); it != queries.end(); ++it)
    {
        query current = *it;

        while(true) {
            const auto right_of_current_it = non_overlapping.lower_bound(current);

            if (right_of_current_it == non_overlapping.end()) {
                // after the last element
                auto inplace = overlap(*non_overlapping.rbegin(), current);
                if (inplace.empty()) {
                    non_overlapping.insert(current);
                    break;
                }

                non_overlapping.erase(*non_overlapping.rbegin());
                current = *inplace.rbegin();
                inplace.erase(current);
                for (auto q: inplace) {
                    non_overlapping.insert(q);
                }
                continue;
            }

            const auto right_of_current = *right_of_current_it;

            if (right_of_current_it == non_overlapping.begin()) {
                // in front of first element
                auto inplace = overlap(current, right_of_current);
                 if (inplace.empty()) {
                    non_overlapping.insert(current);
                    break;
                }

                non_overlapping.erase(right_of_current);
                current = *inplace.rbegin();
                inplace.erase(current);
                for (auto q: inplace) {
                    non_overlapping.insert(q);
                }
                continue;
            }

            if (current.begin == right_of_current.begin) {
                //  it    right_of
                // (2,4)   (2,9)
                auto inplace{ overlap(current, right_of_current) };

                non_overlapping.erase(right_of_current);
                for (auto q: inplace) {
                    non_overlapping.insert(q);
                }
                break;
            }

            // between two elements
            const auto previous = *prev(right_of_current_it);
            if (current.begin <= previous.end) {
                //  prev  it
                // (2,4) (3,6)
                // (2,9) (3,6)
                // (2,4) (2,7)
                non_overlapping.erase(previous);

                auto inplace{ overlap(previous, current) };
                current = *inplace.rbegin();
                inplace.erase(current);
                for (auto q: inplace) {
                    non_overlapping.insert(q);
                }
                continue;
            }

            if (right_of_current.begin <= current.end) {
                non_overlapping.erase(right_of_current);

                auto inplace{ overlap(current, right_of_current) };
                current = *inplace.rbegin();
                inplace.erase(current);
                for (auto q: inplace) {
                    non_overlapping.insert(q);
                }
                continue;
            }

            // otherwise
            non_overlapping.insert(current);
            break;
        }
    }

    long max_value{0};
    for (const auto& q: non_overlapping) {
        if (q.addend > max_value) {
            max_value = q.addend;
        }
    }
    return max_value;
}

void solve() {
    auto [n, queries] = read_input();

    long result = array_manipulation(n, queries);
    show(result);
}

int main() {
    // test_overlap();
    // test_set();
    test_manipulation();
    // solve();
    return EXIT_SUCCESS;
}
