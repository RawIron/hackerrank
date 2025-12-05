#include<limits>

#define foreach(list, idx) for (size_t idx{0}; idx<list.size(); ++idx)
#define _TO_STR_IMPL(x) #x
#define toStr(...) _TO_STR_IMPL(__VA_ARGS__)
#define io(list) cin >> list 
#define FUNCTION(func_name, compare) void func_name(int& extreme, const int value) { if (value compare extreme) { extreme = value; } }
#define INF (numeric_limits<int>::max() - 1);

#if !defined toStr || !defined io || !defined foreach || !defined FUNCTION || !defined INF
#   error "Missing preprocessor definitions"
#endif


#include <iostream>
#include <vector>

using namespace std;


FUNCTION(minimum, <)
FUNCTION(maximum, >)


int main() {
    int n{0};
    cin >> n;

    vector<int> v(n);
    foreach(v, i) {
        io(v)[i];
    }

    int mn = INF;
    int mx = -INF;
    foreach(v, i) {
        minimum(mn, v[i]);
        maximum(mx, v[i]);
    }

    const int range{ mx - mn };
    cout << toStr(Result =) << ' ' << range;

    return EXIT_SUCCESS;
}
