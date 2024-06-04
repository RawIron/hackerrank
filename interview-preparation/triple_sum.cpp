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


void ignore_line() {
    string _ {};
    getline(cin, _);
}

vector<string> split_regex_token(const string& str) {
    regex word_pattern{ R"(\s)" };
    return { sregex_token_iterator(str.cbegin(), str.cend(), word_pattern, -1),
             sregex_token_iterator() };
}

set<int> read_input_set() {
    string line{};
    getline(cin, line);
    
    function<vector<string>(string)> split = split_regex_token;
    const auto words{ split(line) };

    set<int> numbers{};
    auto to_int = [](const string& word) { return stoi(word); };
    transform(words.cbegin(), words.cend(), inserter(numbers, numbers.begin()), to_int);

    return numbers;
}

tuple<size_t, size_t, size_t>
read_sizes() {
    size_t l{};
    size_t m{};
    size_t n{};
    cin >> l >> m >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return make_tuple(l, m, n);
}

template<typename T>
vector<T> read_input(size_t n) {
    vector<T> in_list{};
    for (size_t i{0}; i < n; ++i) {
        T element{};
        cin >> element;    
        in_list.push_back(element);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return in_list;
}


/**
    count the unique triplets that can be constructed such that
        (x,z,y) : a.x <= b.z >= c.y
    
    a = {1,2,3}, b = {2}, c = {2,4}
    (1,2,2), (2,2,2)
*/
long count_triplets_set(const set<int>& a, const set<int>& b, const set<int>& c) {
    long counter{0};

    for (auto&& z : b) {
        const auto
        is_in_a{ a.find(z) != a.end() ? 1 : 0 },
        is_in_c{ c.find(z) != c.end() ? 1 : 0 };
        const auto
        smaller_in_a{ distance(a.begin(), a.lower_bound(z)) + is_in_a },
        smaller_in_c{ distance(c.begin(), c.lower_bound(z)) + is_in_c };

        counter += smaller_in_a * smaller_in_c;
    }

    return counter;
}


vector<int> sort_distinct(const vector<int>& v) {

    vector<int> v_sorted( v.size() );
    copy(v.cbegin(), v.cend(), v_sorted.begin());
    sort(v_sorted.begin(), v_sorted.end());

    vector<int> v_uniq( v.size() );
    const auto v_uniq_end{ unique_copy(v_sorted.cbegin(), v_sorted.cend(), v_uniq.begin()) };
    v_uniq.resize( distance(v_uniq.begin(), v_uniq_end) );

    return v_uniq;
}

long count_triplets(const vector<int>& a, const vector<int>& b, const vector<int>& c) {
    const auto
    a_uniq{ sort_distinct(a) },
    b_uniq{ sort_distinct(b) },
    c_uniq{ sort_distinct(c) };

    long counter{0};
    size_t smaller_in_a{0};
    size_t smaller_in_c{0};

    for (auto&& z : b_uniq) {
        for (auto i{smaller_in_a}; i < a_uniq.size() && a_uniq[i] <= z; i++) {
            ++smaller_in_a;
        }
        for (auto i{smaller_in_c}; i < c_uniq.size() && c_uniq[i] <= z; i++) {
            ++smaller_in_c;
        }
        counter += smaller_in_a * smaller_in_c;
    }

    return counter;
}

bool test_count_triplets() {
    const vector<int> a{1,3,5,7};
    const vector<int> b{5,7,9};
    const vector<int> c{7,9,11,13};

    if (count_triplets(a, b, c) == 12) { return true; }
    else { return false; }
}


void solve_set() {
    ignore_line();

    const auto
    lesser_left{ read_input_set() },
    greater{ read_input_set() },
    lesser_right{ read_input_set() };

    const auto
    counted{ count_triplets_set(lesser_left, greater, lesser_right) };
    show(counted);
}

void solve() {
    const auto [l, m, n] = read_sizes();
    
    const auto
    lesser_left{ read_input<int>(l) },
    greater{ read_input<int>(m) },
    lesser_right{ read_input<int>(n) };

    const auto
    counted{ count_triplets(lesser_left, greater, lesser_right) };
    show(counted);
}


int main() {
    // show( test_count_triplets() );
    // solve_set();
    solve();
    return EXIT_SUCCESS;
}
