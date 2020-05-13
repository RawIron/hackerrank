#include <bits/stdc++.h>
#include <functional>
#if defined(BOOST_VERSION)
#include <boost/algorithm/string.hpp>
#endif

using namespace std;

#if defined(BOOST_VERSION)
vector<string> splitBoost(const string s) {
    vector<string> words{};
    boost::split(words, s, boost::is_any_of(" "), boost::token_compress_on);
    return words;
}
#endif

vector<string> splitStringStream(const string s) {
    // split line into words using string stream 
    stringstream sstream{s};
    string word;
    vector<string> words{};
   
    while (sstream >> word) {
        words.push_back(word);
    }

    return words;
}

vector<string> readInput() {
    int n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    string line{};
    getline(cin, line);

    function<vector<string>(string)> split = splitStringStream;
    return split(line);
}

void show(const vector<string> words) {
    for (auto word : words) {
        cout << word << " ";
    }
    cout << endl;
}

int main()
{
    vector<string> words = readInput();
    reverse(words.begin(), words.end());
    show(words);
    return 0;
}
