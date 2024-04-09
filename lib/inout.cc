#include <iostream>
#include <limits>
#include <string>
#include <sstream>
#include <regex>
#include <vector>
#include <functional>

using namespace std;


#if defined(BOOST_VERSION)
#include <boost/algorithm/string.hpp>
/*
    split a string into words using boost::split
*/
vector<string> split_boost(const string& s) {
    vector<string> words{};
    boost::split(words, s, boost::is_any_of(" "), boost::token_compress_on);
    return words;
}
#endif

/*
    split a string into words using a regex
*/
vector<string> split_regex(const string& s) {
    vector<string> words{};

    const regex word_pattern{R"(\w)"};
    auto text_begin = sregex_iterator(s.cbegin(), s.cend(), word_pattern);
    auto text_end = sregex_iterator();
    for (sregex_iterator it{text_begin}; it != text_end; ++it) {
        words.push_back(it->str());
    }

    return words;
}

/*
    split a string into words using a string stream
*/
vector<string> split_stringstream(const string& s) {
    stringstream sstream{ s };
    vector<string> words{};

    string word{};
    while (sstream >> word) {
        words.push_back(word);
    }

    return words;
}

/*
    cin stream processing

    1  cin >> int
    2  cin >> string >> string >> string >> string
    3  cin >> string
    4  cin.ignore(numeric_limits<streamsize>::max(), '\n');

    4 followed by more \n words   and  ignored  \nHere it continues\n
     ^                         ^     ^            ^
     1                         2     3            4


    1 cin >> int
    2 getline(cin, string)

    4\n not reached
     ^ ^
     1 2
*/


/*
    extracts all words from the second line
    discards the input from the first line

    input line 1: 6
    input line 2: this sentence is six words long
*/
vector<string> read_many_split() {
    int n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    string line{};
    getline(cin, line);

    function<vector<string>(string)> split = split_stringstream;
    return split(line);
}

/*
    reads the number n from cin
    extracts the first n elements from the cin stream

    !side-effect: it consumes the whole stream

    input: 4 this sentence is six words long
    output: {"this", "sentence", "is", "six"}
*/
template <typename T>
vector<T> read_many_istream() {
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

/*
    reads the number n from cin
    extracts the first n elements from the cin stream
*/
template<typename T>
vector<T> read_many_cin() {
    size_t N{};
    cin >> N;

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
T read_one() {
    T n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    return n;
}

template <typename T>
pair<T,T> read_two() {
    T fst{}, snd{};
    cin >> fst;
    cin >> snd;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    return make_pair(fst, snd);
}


/**
    an error in open() is unchecked and causes a panic
    ofstream calls close() in the destructor
*/
template <typename T>
void show_many(const T& items) {
    const char* const out_path{ getenv("OUTPUT_PATH") };
    if (out_path != nullptr) {
        ofstream fout(out_path);
        for (auto& item : items) {
            fout << item << " ";
        }
        fout << endl;
    }
    else {
        for (auto& item : items) {
            cout << item << " ";
        }
        cout << endl;
    }
}


/**
    an error in open() is unchecked and causes a panic
    ofstream calls close() in the destructor
*/
template <typename T>
void show_one(const T value) {
    const char* const out_path{ getenv("OUTPUT_PATH") };
    if (out_path != nullptr) {
        ofstream fout(out_path);
        fout << value << endl;
    }
    else {
        cout << value << endl;
    }
}
