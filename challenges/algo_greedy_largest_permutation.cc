#include <bits/stdc++.h>

using namespace std;

/**
 * build the largest number from digits in a list with at most k swaps
 * list contains unsorted numbers [1..n]
 *
 * swap the largest number to the front
 * take the tail of the list
 * swap the largest number in the tail to the front
 * ...
 *
 * create an index on the values in the list
 * to support a fast lookup of the position of the largest number
 *
 * 2 1 3 4    4 1 3 2    4 3 1 2
 *   2: 0       2: 3       2: 3
 *   1: 1       1: 1       1: 2
 *   3: 2       3: 2       3: 1
 *   4: 3       4: 0       4: 0
 *
 * build_largest 2 [2,1,3,4] == [4,3,1,2]
 */
void build_largest(const int max_swaps, vector<int>& numbers) {
    map<int,int> index_by_num{};
    for(auto i = numbers.begin(); i != numbers.end(); ++i) {
        index_by_num[*i] = i - numbers.begin();
    }

    int swaps{max_swaps};
    size_t current_max{numbers.size()};
    for(auto i = 0; swaps > 0 && i < numbers.size()-1; ++i) {
        if (numbers[i] < current_max) {
            const int current_max_pos{ index_by_num[current_max] };
            index_by_num[numbers[i]] = current_max_pos;
            numbers[current_max_pos] = numbers[i];
            index_by_num[current_max] = i;
            numbers[i] = current_max;
            --swaps;
            --current_max;
        }
        else if (numbers[i] == current_max) {
            --current_max;
        }
    }
}

template <typename T>
T read_one() {
    T n{};
    cin >> n;
    return n;
}

template<typename T>
void read_many(const int N, vector<T>& in_list) {
    for (size_t i{0}; i<N; ++i) {
        T element{};
        cin >> element;
        in_list.push_back(element);
    }
}

template<typename T>
void print_array(const vector<T> items) {
    for(auto item : items) {
        cout << item << " ";
    }
    cout << endl;
}

int main() {
    const int n{ read_one<int>() };
    const int max_swaps{ read_one<int>() };
    vector<int> numbers{};
    read_many<int>(n, numbers);

    build_largest(max_swaps, numbers);

    print_array<int>(numbers);
}
