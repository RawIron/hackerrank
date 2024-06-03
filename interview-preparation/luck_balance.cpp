#include <bits/stdc++.h>

using namespace std;


struct competition {
    size_t can_loose;
    vector<int> important;
    vector<int> unimportant;
};


/**
    maximize luck by loosing no more than a given number
    of important contests
    
    luck is greater than zero
    *   it is best to loose all unimportant contests and
        collect the luck
    *   maximize luck from important contests by mazimizing
        the sum of positive (loss) and negative (win) numbers
*/
long maximize_luck(const competition& contests) {
    const auto
    loose_all_unimportant{ accumulate(contests.unimportant.cbegin(),
                                      contests.unimportant.cend(),
                                      0) };
    
    vector<int> sorted( contests.important.size() );
    copy(contests.important.cbegin(), contests.important.cend(), sorted.begin());
    sort(sorted.begin(), sorted.end());

    const auto
    can_loose{ min(contests.can_loose, contests.important.size()) };
    const auto
    loose_many_of_large_luck{ accumulate(sorted.crbegin(),
                                         sorted.crbegin() + can_loose,
                                         0) },
    win_few_of_small_luck{ accumulate(sorted.crbegin() + can_loose,
                                      sorted.crend(),
                                      0, minus<long>()) };

    const auto
    total_luck{ loose_all_unimportant
                + loose_many_of_large_luck
                + win_few_of_small_luck };

    return total_luck;
}


competition read_input() {
    size_t n{};
    size_t k{};

    cin >> n >> k;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    vector<int> important{};
    vector<int> unimportant{};
    int luck{}, is_important{};

    for (size_t i{0}; i < n; ++i) {
        cin >> luck >> is_important;
        if (is_important) {
            important.push_back(luck);
        }
        else {
            unimportant.push_back(luck);
        }
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    const competition contests{k, important, unimportant};
    return contests;
}

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


int main() {
    auto max_luck = maximize_luck( read_input() );
    show(max_luck);

    return EXIT_SUCCESS;
}
