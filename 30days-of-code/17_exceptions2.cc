#include <cmath>
#include <iostream>
#include <vector>
#include <exception>
#include <stdexcept>

using namespace std;


class Calculator {
    public:
    Calculator() {};

    long power(const int base, const int exponent) const {
        if (base < 0 or exponent < 0) {
            throw invalid_argument("n and p should be non-negative");
        }
        return pow(base, exponent);
    }
};


vector<pair<int,int>> read_number_pairs() {
    int in_lines{};
    cin >>in_lines;

    int n{}, p{};
	vector<pair<int,int>> pairs{};
    while(in_lines-- > 0) {
        if(scanf("%d %d", &n, &p) == 2) {
			pairs.push_back(make_pair(n, p));
        }
    }

	return pairs;
}


pair<int,string> solve(const int n, const int p) noexcept {
    Calculator myCalculator = Calculator();
    try {
        int ans = myCalculator.power(n,p);
		return make_pair(ans, string{});
    }
    catch(exception& e) {
		return make_pair(-1, e.what());
    }
}


int main() {
    for(auto number_pair : read_number_pairs()) {
        auto result = solve(number_pair.first, number_pair.second);

		if (not result.second.empty()) {
			cout <<result.second <<endl;
		}
		else {
			cout <<result.first <<endl;
		}
    }
}
