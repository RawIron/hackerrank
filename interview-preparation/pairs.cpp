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
pair< long, vector<T> >
read_input() {
    size_t N{};
    long k{};
    cin >> N >> k;

    vector<T> in_list{};
    for (size_t i{0}; i < N; ++i) {
        T element{};
        cin >> element;    
        in_list.push_back(element);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return make_pair(k, in_list);
}


long count_pairs(const long distance, const vector<long>& numbers) {
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


int main() {
    auto [distance, numbers] = read_input<long>();
    auto pairs = count_pairs(distance, numbers);
    show(pairs);

    return EXIT_SUCCESS;
}
