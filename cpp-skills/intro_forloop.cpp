#include <iostream>
#include <map>

using namespace std;


string spellDigit(const int n) {
    const map<int, string> spelling {
            {1, "one"},
            {2, "two"},
            {3, "three"},
            {4, "four"},
            {5, "five"},
            {6, "six"},
            {7, "seven"},
            {8, "eight"},
            {9, "nine"},       
        };

    return spelling.at(n);
}

string spellOddOrEven(const int n) {
    if (n % 2 == 0) { return "even"; }
    else { return "odd"; }
}

string solve(const int n) {
    if (n <= 9) {
        return spellDigit(n);
    }
    else {
        return spellOddOrEven(n);
    }
}

int main() {
    int from{}, to{};
    cin >> from;
    cin >> to;

    string answer{};
    for (auto i{from}; i <= to; ++i) {
        answer = solve(i);
        cout << answer << endl;
    }

    return 0;
}
