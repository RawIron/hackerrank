#include <bits/stdc++.h>

using namespace std;


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

template<typename T>
pair< long, set<T> >
read_input() {
    size_t n{};
    long k{};
    cin >> n >> k;

    set<T> in_list{};
    for (size_t i{0}; i < n; ++i) {
        T element{};
        cin >> element;    
        in_list.insert(element);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return make_pair(k, in_list);
}


/**
    two numbers which are _distance_ away from each other are a pair
    count all pairs in a set of numbers

    for any number in a sorted vector search forward for the match
*/
long count_pairs(const long distance, const set<long>& numbers) {
    long pairs{0};

    vector<long> sorted( numbers.size() );
    copy(numbers.cbegin(), numbers.cend(), sorted.begin());
    sort(sorted.begin(), sorted.end());
    auto max_number{ *sorted.crbegin() };

    for (size_t first{0}; first < sorted.size() - 1; ++first) {
        for (size_t second{first + 1}; second < sorted.size(); ++second) {
            const auto target{ sorted[first] + distance };
            if (max_number < target) {
                break;
            }
            if (target < sorted[second]) {
                break;
            }
            if (target == sorted[second]) {
                ++pairs;
                break;
            }
        }
    }

    return pairs;
}

/**
    use a set for fast lookup of `first + distance`
*/
long count_pairs_set(const long distance, const set<long>& numbers) {
    long pairs{0};

    for (const auto first : numbers) {
        const auto target{ first + distance };
        // hackerrank's c++20 is broken
        // const bool is_second_in = numbers.contains(target);
        const bool is_second_in = numbers.find(target) != numbers.end();
        if ( is_second_in ) {
            ++pairs;
        }
    }

    return pairs;
}


int main() {
    auto [distance, numbers] = read_input<long>();
    auto pairs = count_pairs(distance, numbers);
    show(pairs);

    return EXIT_SUCCESS;
}
