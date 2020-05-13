#include <bits/stdc++.h>

using namespace std;

const int loops = 10;

using Equation = tuple<int,int,int>;
using Results = vector<Equation>;


void use_side_effect(const int n) {
    for (int i{1}; i<loops+1; i++) {
        cout << n << " x " << i << " = " << n * i << endl;
    }
}

Results use_for(const int n) {
    Results multiplied{};
    for (int i{1}; i<loops+1; i++) {
        multiplied.push_back(Equation (n,i,n*i));
    }
    return multiplied;
}

void show(const Results results) {
    for (auto r : results) {
        cout << get<0>(r) << " x " << get<1>(r) << " = " << get<2>(r) << endl;
    }
}

int main()
{
    int n;
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    show(use_for(n));

    return 0;
}
