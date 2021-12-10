#include <bits/stdc++.h>
#include <cstddef>

using namespace std;

/**
 * count the steps required to change all uneven numbers in a list to even
 * with the only operation allowed is to increment two adjacent elements by 1 
 *     1 2  ->  2 3
 *     3 3  ->  4 4
 *
 * requires 2 uneven numbers to convert both to even
 * number of steps required (pos_higher - pos_lower)
 *   with one step the lower uneven is moved 1 position closer to the higher uneven
 *   when the lower is beside the higher uneven it takes 1 step to convert both to even
 * costs per step is 2
 *
 * examples:
 *     1 4 4 5
 *     2 5 4 5
 *     2 6 5 5
 *     2 6 6 6
 *     steps == 3 - 0
 * only solvable if count of uneven numbers is even
 *     1 4 5 4 7
 *     2 5 5 4 7
 *     2 6 6 4 7
 */
pair<int, bool> count_steps(const vector<int>& numbers) {
    constexpr int cost_per_step{ 2 };
    int total_steps{ 0 };
    bool is_uneven{ false };
    struct {
        bool have_one;
        size_t at;
    } uneven{ false, 0 };

    for (auto i=0; i < numbers.size(); ++ i) {
        is_uneven = (numbers[i] % 2 == 1);
        if (is_uneven && uneven.have_one) {
            total_steps += (i - uneven.at);
            uneven.have_one = false;
        } else if (is_uneven) {
            uneven.have_one = true;
            uneven.at = i;
        }
    }

    return make_pair(total_steps * cost_per_step, !uneven.have_one);
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

int main() {
    const int n{ read_one<int>() };
    vector<int> numbers{};
    read_many<int>(n, numbers);

    const auto result{ count_steps(numbers) };

    if (result.second) {
        cout << result.first << endl;
    } else {
        cout << "NO" << endl;
    }
}
