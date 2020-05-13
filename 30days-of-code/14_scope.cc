#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;


class Difference {
    private:
    const vector<int> elements;
  	int maximum;
	bool computed;

	public:
    explicit Difference(const vector<int> numbers)
        : elements{numbers}, maximum{0}, computed{false}
    {}

    int compute() {
		if (computed) { return maximum; }

        pair<vector<int>::iterator, vector<int>::iterator> minmax = std::minmax_element(elements.begin(), elements.end());
        maximum = *minmax.second - *minmax.first;
		computed = true;
		return maximum;
    }

};


vector<int> readInput() {
    int N{};
    cin >> N;

    vector<int> numbers{};
    for (int i{0}; i<N; ++i) {
        int number{};
        cin >> number;
        
        numbers.push_back(number);
    }

	return numbers;
}


int main() {
	vector<int> numbers{readInput()};

    Difference diff{numbers};
    cout << diff.compute() << endl;

    return 0;
}
