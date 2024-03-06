#include <bits/stdc++.h>
#include <streambuf>

using namespace std;

/**
   count occurences of letters in a word

   frequencies("abba") == {{'a',2}, {'b',2}}
 */
map<char,int> frequencies(const string& word) {
    map<char,int> freq{};

    for (const char c : word) {
        ++freq[c];
    }

    return freq;
}

/**
   count the number of deletes which are required to
   make the count of letter occurences in both words equal

   subtract counts of smaller word from larger word
   sum abs(updated count of larger word)

     bbadd abbac
     bba    bba 

   a: 1     -2   -1
   b: 2     -2    0
   c: 0     -1   -1
   d: 2           2
                ---
                  3

   make_anagram("abbac", "bbadd") == 4
 */
int make_anagram(const string& fst_word, const string& snd_word) {
    const bool isFstWordSmaller{ fst_word.length() <= snd_word.length() };
    auto longer_word{ isFstWordSmaller ? snd_word : fst_word };
    auto shorter_word{ isFstWordSmaller ? fst_word : snd_word };

    auto freq{ frequencies(longer_word) };
    for (const char c : shorter_word) {
        --freq[c];
    }

    return accumulate(cbegin(freq), cend(freq), 0,
            [](const int total_deletes, const auto& letter_freq) {
                return total_deletes + abs(letter_freq.second); });
}

template <typename T>
void show(T result) {
    const char* const out_path{ getenv("OUTPUT_PATH") };
    if (out_path != nullptr) {
        ofstream fout(out_path);
        fout << result << endl;
    } else {
        cout << result << endl;
    }
}

pair<string, string>
read_input() {
    string s1{};
    getline(cin, s1);
    string s2{};
    getline(cin, s2);

    return make_pair(s1, s2);
}

int main() {
    auto [s1, s2] = read_input();
    const int result = make_anagram(s1, s2);
    show(result);

    return EXIT_SUCCESS;
}
