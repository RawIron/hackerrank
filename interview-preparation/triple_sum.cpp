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

set<int> read_input() {
    string line{};
    getline(cin, line);
    
    function<vector<string>(string)> split = split_regex_token;
    const auto words{ split(line) };

    set<int> numbers{};
    auto to_int = [](const string& word) { return stoi(word); };
    transform(words.cbegin(), words.cend(), inserter(numbers, numbers.begin()), to_int);

    return numbers;
}


long count_triplets(const set<int>& a, const set<int>& b, const set<int>& c) {
    long counter{0};

    for (auto&& n : b) {
        const auto
        is_in_a{ a.find(n) != a.end() ? 1 : 0 },
        is_in_c{ c.find(n) != c.end() ? 1 : 0 };
        const auto
        smaller_in_a{ distance(a.begin(), a.lower_bound(n)) + is_in_a },
        smaller_in_c{ distance(c.begin(), c.lower_bound(n)) + is_in_c };

        counter += smaller_in_a * smaller_in_c;
    }

    return counter;
}

void test_count_triplets() {
    const set<int> a{1,3,5,7};
    const set<int> b{5,7,9};
    const set<int> c{7,9,11,13};
    show(count_triplets(a, b, c));
}


void solve() {
    ignore_line();
    const auto lesser_left{ read_input() };
    const auto greater{ read_input() };
    const auto lesser_right{ read_input() };

    const auto counted{ count_triplets(lesser_left, greater, lesser_right) };
    show(counted);
}


int main() {
    // test_count_triplets();
    solve();
    return EXIT_SUCCESS;
}
