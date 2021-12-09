#include <bits/stdc++.h>

using namespace std;

/**
 * calculate the maximum number of pairs for which:
 *   both lists have the same number of elements
 *   a pair contains one element from each list
 *   any element can only be matched once
 *   one element in the second list _must_ be changed
 *
 * match elements from first and second list
 * if there is more than one element which could be used
 *   for a match choose any
 *   1 2 2   1 2 2
 *    /  |      X
 *   |   |    /  |
 *   2 1 2   2 1 2
 * either all elements have a match
 *   or some elements are not part of a pair
 *   1 2 4 1    1 2 4 1
 *   4 1 2 1    2 2 4 1
 * because the number of elements in the lists are the same
 *   any not matched element in one list
 *   has a not matched element in the other list
 *
 * one element _must_ be changed
 *   if there are not matched elements: create one more match
 *                           otherwise: one pair is lost
 *
 * max_pairs [1] [1] == 0
 * max_pairs [1,2] [2,3] == 2
 */
int max_pairs(vector<int>& fst_list, vector<int>& snd_list) {
    map<int,int> freq{};
    for(auto elem : snd_list) {
        ++freq[elem];
    }

    int pairs{ 0 };
    int not_matched{ 0 };
    map<int, int>::iterator it{};
    for(auto elem : fst_list) {
        it = freq.find(elem);
        if (it != freq.end() && it->second > 0) {
            --(it->second);
            ++pairs;
        }
        else {
            ++not_matched;
        }
    }

    if (not_matched > 0)
        return ++pairs;
    else
        return --pairs;
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
    vector<int> fst_list{};
    read_many<int>(n, fst_list);
    vector<int> snd_list{};
    read_many<int>(n, snd_list);

    const int max_size{ max_pairs(fst_list, snd_list) };

    cout << max_size << endl;
}
