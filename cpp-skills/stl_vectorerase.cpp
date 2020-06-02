#include <vector>
#include <iostream>
#include <algorithm>
#include <functional>

using namespace std;


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
vector<T> readMany() {
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


int main() {
    // read | erase | show
    vector<int> numbers{ readMany<int>() };
    int offset{ readOne<int>() };
    int rstart{}, rend{};
    tie(rstart, rend) = readTwo<int>();

    // offset and range index starting with 1, and not 0
    // range is [rstart, rend)
    numbers.erase(numbers.begin() + offset-1);
    numbers.erase(numbers.begin() + rstart-1, numbers.begin() + rend-1);

    function<void(vector<int>)> show = showMany<vector<int>>;
    cout << numbers.size() << endl;
    show(numbers);
    
    return 0;
}
