#include <iostream>
#include <deque>
#include <vector>
#include <algorithm>

using namespace std;


void findMaxInSubsets(const int arr[], const size_t n, const size_t k, deque<int>& maxSubsets) {
    deque<int> subset{};

    for (size_t i{0}; i<k; ++i) {
        subset.push_front(arr[i]);
    }
    maxSubsets.push_front(*max_element(subset.begin(), subset.end()));

    for (size_t i{k}; i<n; ++i) {
        subset.push_front(arr[i]);
        const bool maxIsIn{ subset.back() < maxSubsets.front() };
        const bool newItemIsMax{ subset.front() > maxSubsets.front() };
        subset.pop_back();
        if (maxIsIn) {
            if (newItemIsMax) {
                maxSubsets.push_front(subset.front());
            }
            else {
                maxSubsets.push_front(maxSubsets.front());
            }
        }
        else {
            maxSubsets.push_front(*max_element(subset.begin(), subset.end()));
        }
    }
}

void show(const deque<int>& values) {
    for (auto it{values.rbegin()}; it != values.rend(); ++it ) {
        cout << *it << " ";
    }
    cout << endl;
}


int main() {
    int t{};
    cin >> t;

    for (int i{0}; i<t; ++i) {
        size_t n{}, k{};
        cin >> n >> k;

        int arr[n]{};
        for(size_t i{0}; i<n; ++i) {
              cin >> arr[i];
        }

        deque<int> maxInSubsets{};
        findMaxInSubsets(arr, n, k, maxInSubsets);

        show(maxInSubsets);
      }

      return 0;
}
