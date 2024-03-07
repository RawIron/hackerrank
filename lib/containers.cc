#include <map>
#include <string>

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
   count occurences of each item in a list of items

   frequencies({"the", "car", "the"}) == {{"the",2}, {"car",1}}
 */
template<typename T>
map<T,int> frequencies(const T& items) {
    map<T,int> freq{};

    for (const T& item : items) {
        ++freq[item];
    }

    return freq;
}
