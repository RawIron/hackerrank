#include <bits/stdc++.h>

using namespace std;

/**
 * count occurences of letters in a word
 *
 * frequencies("abba", {}) == {{'a',2}, {'b',2}}
 */
void frequencies(const string word, map<char,int>& freq) {
    for (auto c : word) {
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
 * makeAnagram("abba", "bbadd") == 3
 */
int makeAnagram(const string fst_word, const string snd_word) {
    const bool isFstWordSmaller{ fst_word.length() <= snd_word.length() };
    string map_word{ isFstWordSmaller ? snd_word : fst_word };
    string probe_word{ isFstWordSmaller ? fst_word : snd_word };

    map<char, int> freq{};
    frequencies(map_word, freq);
    for (auto c : probe_word) {
        --freq[c];
    }

    return accumulate(begin(freq), end(freq), 0,
            [](const int acc, const auto& element) { return acc + abs(element.second); });
}

int main() {
    string s1{};
    getline(cin, s1);
    string s2{};
    getline(cin, s2);

    const int result = makeAnagram(s1, s2);
    
    ofstream fout(getenv("OUTPUT_PATH"));
    fout << result << endl;
    fout.close();

    return 0;
}
