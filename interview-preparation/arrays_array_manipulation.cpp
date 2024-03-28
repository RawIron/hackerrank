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
    two points

    id
    ab  cd

    swallow
    abcd            a++d
*/
vector<query> overlap_ab_cd(const query ab, const  query cd) {
    vector<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(a == b && c == d);

    if (a == c) {
        // swallow
        const query ad{a, d, ab.addend + cd.addend};
        pieces.push_back(ad);
    }

    if (a != c) {
        // id
        pieces.push_back(ab);
        pieces.push_back(cd);
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
vector<query> overlap_ab(const query ab, const  query cd) {
    vector<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(c < d);
    assert(a == b);

    if (a < c) {
        // id
        pieces.push_back(ab);
        pieces.push_back(cd);
    }
    if (d < a) {
        // id
        pieces.push_back(cd);
        pieces.push_back(ab);
    }

    if (c < a && a < d) {
        const query ca{c, a-1, cd.addend};
        const query aa{a, a, ab.addend + cd.addend};
        const query ad{a+1, d, cd.addend};

        pieces.push_back(ca);
        pieces.push_back(aa);
        pieces.push_back(ad);
    }

    if (c == a) {
        const query ac{a, a, ab.addend + cd.addend};
        const query bd{a+1, d, cd.addend};

        pieces.push_back(ac);
        pieces.push_back(bd);
    }

    if (a == d) {
        const query ca{c, a-1, cd.addend};
        const query bb{a, a, ab.addend + cd.addend};

        pieces.push_back(ca);
        pieces.push_back(bb);
    }

    return pieces;
}


/*
    two intervals with length > 0
    (!) expects
        ab.begin == cd.begin
    
    in case (ab.end == cd.end)
        multiply ab and cd with (-1)
        swap begin and end

    swallow
    ac---bd         a++d
    ac__d--b        ac++d  d--b
    ac--b__d        ac++b  b__d
*/
vector<query> overlap_ac(const query ab, const  query cd) {
    vector<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(a < b);
    assert(c < d);
    assert(a == c);

    if (b == d) {
        const query ad{a, d, ab.addend + cd.addend};
        pieces.push_back(ad);
    }

    if (b > d) {
        const query ad{a, d, ab.addend + cd.addend};
        const query db{d+1, b, ab.addend};

        pieces.push_back(ad);
        pieces.push_back(db);
    }

    if (b < d) {
        const query cb{a, b, ab.addend + cd.addend};
        const query bd{b+1, d, cd.addend};

        pieces.push_back(cb);
        pieces.push_back(bd);
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
*/
vector<query> overlap_a_c(const query ab, const  query cd) {
    vector<query> pieces{};

    const auto a{ ab.begin };
    const auto b{ ab.end };
    const auto c{ cd.begin };
    const auto d{ cd.end };

    assert(a < b);
    assert(c < d);
    assert(a < c);
    assert(b != d);

    if (b < c) {
        // id
        pieces.push_back(ab);
        pieces.push_back(cd);
    }

    if (c < b && d < b) {
        // swallow
        const query ac{a, c-1, ab.addend};
        const query cd_{c, d, ab.addend + cd.addend};
        const query db{d+1, b, ab.addend};

        pieces.push_back(ac);
        pieces.push_back(cd_);
        pieces.push_back(db);
    }

    if (c < b && b < d) {
        // overlap
        const query ac{a, c-1, ab.addend};
        const query cb{c, b, cd.addend + ab.addend};
        const query bd{b+1, d, cd.addend};

        pieces.push_back(ac);
        pieces.push_back(cb);
        pieces.push_back(bd);
    }

    return pieces;
}


void test_out(bool pass) {
    if (pass) {
        cout << "P";
    }
    else {
        cout << "F";
    }
}

void test_overlap_ab_cd() {
    {
    const query ab{2, 2, 5};
    const query cd{7, 7, 1};
    auto result = overlap_ab_cd(ab, cd);
    const vector<query> expected{ {2,2,5}, {7,7,1} };
    test_out(result == expected);
    }
    {
    const query ab{2, 2, 5};
    const query cd{2, 2, 1};
    auto result = overlap_ab_cd(ab, cd);
    const vector<query> expected{ {2,2,6} };
    test_out(result == expected);
    }
}

void test_overlap_ab() {
    {
    const query ab{2, 2, 5};
    const query cd{4, 7, 1};
    auto result = overlap_ab(ab, cd);
    const vector<query> expected{ {2,2,5}, {4,7,1} };
    test_out(result == expected);
    }
    {
    const query ab{8, 8, 5};
    const query cd{4, 7, 1};
    auto result = overlap_ab(ab, cd);
    const vector<query> expected{ {4,7,1}, {8,8,5} };
    test_out(result == expected);
    }
    {
    const query ab{2, 2, 5};
    const query cd{1, 4, 1};
    auto result = overlap_ab(ab, cd);
    const vector<query> expected{ {1,1,1}, {2,2,6}, {3,4,1} };
    test_out(result == expected);
    }
    {
    const query ab{1, 1, 5};
    const query cd{1, 4, 1};
    auto result = overlap_ab(ab, cd);
    const vector<query> expected{ {1,1,6}, {2,4,1} };
    test_out(result == expected);
    }
    {
    const query ab{4, 4, 5};
    const query cd{1, 4, 1};
    auto result = overlap_ab(ab, cd);
    const vector<query> expected{ {1,3,1}, {4,4,6} };
    test_out(result == expected);
    }
}

void test_overlap_ac() {
    {
    const query ab{2, 4, 5};
    const query cd{2, 7, 1};
    auto result = overlap_ac(ab, cd);
    const vector<query> expected{ {2,4,6}, {5,7,1} };
    test_out(result == expected);
    }
    {
    const query ab{2, 7, 5};
    const query cd{2, 4, 1};
    auto result = overlap_ac(ab, cd);
    const vector<query> expected{ {2,4,6}, {5,7,5} };
    test_out(result == expected);
    }
    {
    const query ab{2, 4, 5};
    const query cd{2, 4, 1};
    auto result = overlap_ac(ab, cd);
    const vector<query> expected{ {2,4,6} };
    test_out(result == expected);
    }
}

void test_overlap_a_c() {
    {
    const query ab{2, 4, 5};
    const query cd{5, 7, 1};
    auto result = overlap_a_c(ab, cd);
    const vector<query> expected{ {2,4,5}, {5,7,1} };
    test_out(result == expected);
    }
    {
    const query ab{2, 7, 5};
    const query cd{5, 8, 1};
    auto result = overlap_a_c(ab, cd);
    const vector<query> expected{ {2,4,5}, {5,7,6}, {8,8,1} };
    test_out(result == expected);
    }
    {
    const query ab{2, 8, 5};
    const query cd{5, 7, 1};
    auto result = overlap_a_c(ab, cd);
    const vector<query> expected{ {2,4,5}, {5,7,6}, {8,8,5} };
    test_out(result == expected);
    }
}

void test_overlap() {
    test_overlap_ac();
    test_overlap_ab_cd();
    test_overlap_ab();
    test_overlap_a_c();
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

void solve() {
    auto [n, queries] = read_input();

    long result = array_manipulation(n, queries);
    show(result);
}

int main() {
    // test_overlap();
    solve();
    return EXIT_SUCCESS;
}
