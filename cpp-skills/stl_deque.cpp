#include <iostream>
#include <deque>
#include <vector>
#include <algorithm>

using namespace std;


/**
 * Solution 2
 *
 * code uses the language of the problem domain
 * the lower level operations on the collections have been moved into functions
 * function abstractions name a step in the problem solving process
 * it is more readable on the higher level of abstraction
 * understanding how it is implemented requires jumping back and forth in the code
 * for this tiny problem there are many extra lines of code
 *
 */
class MaxInSubset {
public:
    explicit MaxInSubset(const size_t k_) : isFirstComplete{false}, k{k_}, subset{}, maxSubsets{} {}

    void send(const int item) {
        if (not isFirstComplete) {
            insert(item);
            if (isSizeK()) {
                storeFirstMax();
                isFirstComplete = true;
            }
        }
        else {
            const int itemLeft{ itemLeaving() };
            insert(item);
            erase();
            storeNextMax(itemLeft);
        }
    }

    const deque<int>& retrieve() const {
        return maxSubsets;
    }

private:
    void storeFirstMax() {
        store(maxItem());
    }

    void storeNextMax(const int itemLeft) {
        const bool maxIsIn{ itemLeft < previousMax() };

        if (maxIsIn) {
            const bool newItemIsMax{ newItem() > previousMax() };
            if (newItemIsMax) {
                store(newItem());
            }
            else {
                store(previousMax());
            }
        }
        else {
            store(maxItem());
        }
    }

    void store(const int item) { maxSubsets.push_front(item); }
    int previousMax() const { return maxSubsets.front(); }

    int newItem() const { return subset.front(); }
    int itemLeaving() const { return subset.back(); }
    int maxItem() const { return *max_element(subset.cbegin(), subset.cend()); }
    bool isSizeK() const { return subset.size() == k; }

    void insert(const int item) { subset.push_front(item); }
    void erase() { subset.pop_back(); }

private:
    bool isFirstComplete;
    const size_t k;
    deque<int> subset;
    deque<int> maxSubsets;
};

void findMaxInSubsetsAdt(const int arr[], const size_t n, const size_t k, deque<int>& maxSubsets) {
    MaxInSubset adt{k};
    for (size_t i{0}; i<n; ++i) {
        adt.send(arr[i]);
    }
    maxSubsets = adt.retrieve();
}


/**
 * Solution 1
 *
 * implemented without abstractions
 * the code only shows the operations on the deque collection
 * short and concise, no need to jump back and forth while reading
 * this is not easy to read
 * the reader has to extract the problem solving steps from the deque operations
 *
 */
void findMaxInSubsets(const int arr[], const size_t n, const size_t k, deque<int>& maxSubsets) {
    deque<int> subset{};

    for (size_t i{0}; i<k; ++i) {
        subset.push_front(arr[i]);
    }
    maxSubsets.push_front(*max_element(subset.cbegin(), subset.cend()));

    for (size_t i{k}; i<n; ++i) {
        subset.push_front(arr[i]);
        const bool maxIsIn{ subset.back() < maxSubsets.front() };
        subset.pop_back();
        if (maxIsIn) {
            const bool newItemIsMax{ subset.front() > maxSubsets.front() };
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

        int arr[n];
        for(size_t i{0}; i<n; ++i) {
              cin >> arr[i];
        }

        deque<int> maxInSubsets{};
        findMaxInSubsetsAdt(arr, n, k, maxInSubsets);

        show(maxInSubsets);
      }

      return 0;
}
