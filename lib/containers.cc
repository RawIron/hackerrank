#include <vector>
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
map<T,int> frequencies(const vector<T>& items) {
    map<T,int> freq{};

    for (const T& item : items) {
        ++freq[item];
    }

    return freq;
}


/**
 * in progress ..
 */
void copy_while(set<query>::iterator source_it,
                set<query>::iterator source_end,
                insert_iterator<set<query>> dest_it,
                function<bool(query)> lambda)
{
    while (source_it != source_end && lambda(*source_it)) {
        *dest_it = *source_it;
        ++source_it;
        ++dest_it;
    }
    return;
}
