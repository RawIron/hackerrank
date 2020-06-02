#include <iostream>
#include <limits>
#include <string>
#include <sstream>
#include <regex>
#include <vector>
#include <functional>

using namespace std;


#if defined(BOOST_VERSION)
vector<string> splitBoost(const string s) {
    // split a string into words using boost::split
    vector<string> words{};
    boost::split(words, s, boost::is_any_of(" "), boost::token_compress_on);
    return words;
}
#endif

vector<string> splitRegex(const string s) {
    // split a string into words using regex
    vector<string> words{};

    regex word_pattern{R"(\w)"};
    auto words_begin = sregex_iterator(s.begin(), s.end(), word_pattern);
    auto words_end = sregex_iterator();
    for (sregex_iterator it{words_begin}; it != words_end; ++it) {
        words.push_back(it->str());
    }

    return words;
}

vector<string> splitStringStream(const string s) {
    // split a string into words using string stream 
    stringstream sstream{s};
    vector<string> words{};

    string word{};
    while (sstream >> word) {
        words.push_back(word);
    }

    return words;
}

vector<string> readManyWithSplit() {
    int n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    string line{};
    getline(cin, line);

    function<vector<string>(string)> split = splitStringStream;
    return split(line);
}


template <typename T>
vector<T> readManyIstream() {
    // extracts the first n elements from the cin stream
    // it consumes the whole stream !!
	int n{};
    cin >> n;

    vector<T> items{};
    copy_if(
        istream_iterator<T>(cin),
        istream_iterator<T>(),
        back_inserter(items),
        [count=n] (T) mutable { return count && count--; }
    );
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return items;
}


template <typename T>
T readOne() {
    T n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    return n;
}

template <typename T>
pair<T,T> readTwo() {
    T fst{}, snd{};
    cin >> fst;
    cin >> snd;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    return make_pair(fst, snd);
}


template<typename T>
vector<T> readManyFromSingleLine() {
    size_t N{};
    cin >> N;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    vector<T> in_list{};
    for (size_t i{0}; i<N; ++i) {
        T element{};
        cin >> element;
        
        in_list.push_back(element);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return in_list;
}


template <typename T>
void showMany(const T items) {
    for (auto& item : items) {
        cout << item << " ";
    }
    cout << endl;
}

template <typename T>
void showStreamOne(const T value) {
    ofstream fout(getenv("OUTPUT_PATH"));
    fout << value << "\n";
    fout.close();  
}
