#include<limits>

#define FOREACH(list, idx) for (size_t idx{0}; idx<list.size(); ++idx)
#define _TO_STR_IMPL(x) #x
#define TO_STR(...) _TO_STR_IMPL(__VA_ARGS__)
#define IO(list) cin >> list
#define FUNCTION(func_name, compare) void func_name(int& extreme, const int value) { if (value compare extreme) { extreme = value; } }
#define INF (numeric_limits<int>::max() - 1);

#if !defined TO_STR || !defined IO || !defined FOREACH || !defined FUNCTION || !defined INF
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
    FOREACH(v, i) {
        IO(v)[i];
    }

    int mn = INF;
    int mx = -INF;
    FOREACH(v, i) {
        minimum(mn, v[i]);
        maximum(mx, v[i]);
    }

    const int range{ mx - mn };
    cout << TO_STR(Result =) << ' ' << range << endl;

    return EXIT_SUCCESS;
}
