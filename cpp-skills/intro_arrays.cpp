#include <iostream>
#include <algorithm>
#include <regex>

using namespace std;


template<typename T>
vector<T> readInput() {
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


void solve_riterator(vector<int> numbers) {
    for (auto it{numbers.rbegin()}; it<numbers.rend(); ++it) {
        cout << *it << " ";
    }
    cout << endl;
}

void solve_reverse(vector<int> numbers) {
    reverse(numbers.begin(), numbers.end());
    for (const auto number : numbers) {
        cout << number << " ";
    }
    cout << endl;
}


int main() {
    vector<int> numbers = readInput<int>();
    solve_riterator(numbers);

    return 0;
}
