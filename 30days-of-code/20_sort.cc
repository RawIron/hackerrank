#include <vector>
#include <iostream>
#include <algorithm>
#include <functional>

using namespace std;

using SortFunction = function<pair<vector<int>,size_t>(const vector<int>)>;


template<typename T>
pair<vector<T>,size_t> bubbleSortOutOfPlace(vector<T> list) {
    size_t swapCounter{};
    size_t totalSwaps{0};

    for (size_t i{0}; i<list.size(); ++i) {
        swapCounter = 0;
        for(auto it = std::begin(list)+1; it != std::end(list); ++it) {
            if (*(it-1) > *it) {
               iter_swap(it-1, it);
               swapCounter += 1;
            }
        }
        totalSwaps += swapCounter;
        if (swapCounter == 0) {
            break;
        }
    }
    return make_pair(list, totalSwaps);
}

template<typename T>
vector<T> readMany() {
    size_t N{};
    cin >> N;

    vector<T> in_list{};
    for (size_t i{0}; i<N; ++i) {
        T element{};
        cin >> element;
        
        in_list.push_back(element);
    }

    return in_list;
}

int main() {
    // readMany | sort | show
    vector<int> numbers{ readMany<int>() };

    SortFunction mySort = bubbleSortOutOfPlace<int>;
    pair<vector<int>, size_t> result{};

    result = mySort(numbers);
    vector<int> sortedNumbers = result.first;
    size_t totalSwaps = result.second;

    cout <<"Array is sorted in " <<totalSwaps <<" swaps." <<endl;
    cout <<"First Element: " <<sortedNumbers.front() <<endl;
    cout <<"Last Element: " <<sortedNumbers.back() <<endl;
    return 0;
}
