#include <bits/stdc++.h>

using namespace std;


/**
    reads the number n from cin
    extracts the first n elements from the cin stream
*/
template<typename T>
vector<T> read_input() {
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

/**
    an error in open() is unchecked and causes a panic
    ofstream calls close() in the destructor
*/
template <typename T>
void show(const T value) {
    const char* const out_path{ getenv("OUTPUT_PATH") };
    if (out_path != nullptr) {
        ofstream fout(out_path);
        fout << value << endl;
    }
    else {
        cout << value << endl;
    }
}


/**
    find the smallest distance between any two numbers
    in a list of numbers

    in a sorted list the smallest distance is always
    the distance to a neighbor
 */
long min_abs_distance(const vector<long>& numbers) {
    auto sorted{ numbers };
    sort(sorted.begin(), sorted.end());

    long min_distance{ numeric_limits<long>::max() };
    for (size_t i{1}; i < sorted.size(); ++i) {
        auto distance{ sorted[i] - sorted[i-1] };
        if (distance < min_distance) {
            min_distance = distance;
        }
    }
    
    return min_distance;
}

/**
    solution with iterators
*/
long min_abs_distance_iter(vector<long>::const_iterator first,
                           vector<long>::const_iterator last)
{
    vector<long> sorted( distance(first,last) );
    copy(first, last, sorted.begin());
    sort(sorted.begin(), sorted.end());

    long min_distance{ numeric_limits<long>::max() };
    for (auto it{sorted.cbegin()+1}; it < sorted.cend(); ++it) {
        auto distance{ *it  - *(it-1) };
        if (distance < min_distance) {
            min_distance = distance;
        }
    }
    
    return min_distance;
}

/**
    solution with adjacent_difference
*/
long min_abs_distance_adjacent(vector<long>::const_iterator first,
                           vector<long>::const_iterator last)
{
    vector<long> sorted( distance(first,last) );
    copy(first, last, sorted.begin());
    sort(sorted.begin(), sorted.end());

    vector<long> differences( distance(first,last) );
    adjacent_difference(sorted.cbegin(), sorted.cend(), differences.begin());

    auto min_distance = *min_element(differences.cbegin()+1, differences.cend());

    return min_distance;
}


int main() {
    auto numbers{ read_input<long>() };
    // auto min_distance = min_abs_distance(numbers);
    auto min_distance = min_abs_distance_iter(numbers.cbegin(), numbers.cend());
    show(min_distance);

    return EXIT_SUCCESS;
}
