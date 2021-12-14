#include <bits/stdc++.h>
#include <streambuf>

using namespace std;

/**
 * count occurences of letters in a word
 *
 * frequencies("abba", {}) == {{'a',2}, {'b',2}}
 */
void frequencies(const string word, map<char,int>& freq) {
    for (const auto c : word) {
        ++freq[c];
    }
}

/**
 * count the number of deletes which are required to
 * make the count of letter occurences in both words equal
 *
 * subtract counts of smaller word from larger word
 * sum abs(updated count of larger word)
 *
 *   bbadd abba
 *   bba    bba 
 *
 * a: 1     -2   -1
 * b: 2     -2    0
 * d: 2           2
 *              ---
 *                3
 *
 * make_anagram("abba", "bbadd") == 3
 */
int make_anagram(const string fst_word, const string snd_word) {
    const bool isFstWordSmaller{ fst_word.length() <= snd_word.length() };
    const string map_word{ isFstWordSmaller ? snd_word : fst_word };
    const string probe_word{ isFstWordSmaller ? fst_word : snd_word };

    map<char, int> freq{};
    frequencies(map_word, freq);
    for (const auto c : probe_word) {
        --freq[c];
    }

    return accumulate(cbegin(freq), cend(freq), 0,
            [](const int acc, const auto& element) { return acc + abs(element.second); });
}


template <typename T>
void write_fbuf(T result) {
    streambuf* buf{};

    const char* const out_path{ getenv("OUTPUT_PATH") };
    if (out_path != nullptr) {
        filebuf fbuf{};
        fbuf.open(out_path, ios_base::out);
        buf = &fbuf;
    } else {
        buf = cout.rdbuf();
    }

    ostream fout(buf);
    fout << result << endl;
}

template <typename T>
void write(T result) {
    const char* const out_path{ getenv("OUTPUT_PATH") };
    if (out_path != nullptr) {
        ofstream fout(out_path);
        fout << result << endl;
        fout.close();
    } else {
        cout << result << endl;
    }
}

int main() {
    string s1{};
    getline(cin, s1);
    string s2{};
    getline(cin, s2);

    const int result = make_anagram(s1, s2);

    write(result);

    return 0;
}
