#include <bits/stdc++.h>

using namespace std;

template<typename T>
vector<T> read_many() {
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
void show(const T value) {
    ofstream fout(getenv("OUTPUT_PATH"));
    fout << value << "\n";
    fout.close();  
}

/**
    calculate minimum swaps required to sort the input ascending
    (!) side-effect: in-place sorting
    
    @param numbers a vector of size N containing the numbers 1..N
*/
int minimum_swaps(vector<int>& numbers) {
    int swaps{0};

    // in the sorted vector the number at index i _must be_ i+1
    // move what is at index i to the index where i+1 was found
    // worst case: N-1 swaps are required
    for (size_t i{0}; i < numbers.size() - 1; ++i) {
        if (numbers[i] != i+1) {
            const int tmp = numbers[i];
            numbers[i] = i+1;
            const size_t swap_with = find(numbers.cbegin() + (i+1), 
                                          numbers.cend(),
                                          i+1)
                                     - numbers.cbegin();
            numbers[swap_with] = tmp;
            ++swaps;
        }
    }
    
    return swaps;
}

int main() {
    vector<int> numbers{read_many<int>()};

    show(minimum_swaps(numbers));

    return EXIT_SUCCESS;
}
