#include <bits/stdc++.h>
#include <cassert>

using namespace std;


template <typename T>
T readOne() {
    T n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    return n;
}


string ConvertUseBitSet(const uint_fast32_t number) {
	bitset<32> bits{ number };
    string binary{ bits.to_string() };
	binary.erase(0, binary.find_first_not_of("0"));
	return binary;
}

string ConvertUseMod(uint_fast32_t number) {
	string binary{""};

	do
		if (number % 2 != 0) {
			binary += "1";
		}
		else {
			binary += "0";
		}
	while (number /= 2);
    reverse(binary.begin(), binary.end());

    return binary;
}

string ConvertUseBitOp(uint_fast32_t number) {
    string binary{""};

    do {
        if (number & 1) {
            binary += "1";
        } else {
            binary += "0";
        }
    } while (number >>= 1);
    reverse(binary.begin(), binary.end());

    return binary;
}

void testConvertsAreEqualFor(uint_fast32_t number) {
	assert(ConvertUseBitOp(number) == ConvertUseMod(number));
	assert(ConvertUseMod(number) == ConvertUseBitSet(number));
}

int CountUseBitOp(uint_fast32_t number) {
    int longestStreak{0}, streak{0};

    do {
        if (number & 1) {
            ++streak;
        } else {
            longestStreak = max(longestStreak, streak);
            streak = 0;
        }
    } while (number >>= 1);
    // 0000111011011
    longestStreak = max(longestStreak, streak);

    return longestStreak;
}

tuple<string, short> solveUseBitOp() {
    // input is between 1 and 10^6
    uint_fast32_t n = readOne<uint_fast32_t>();

	testConvertsAreEqualFor(n);

    function<string(uint_fast32_t)> int2binary = ConvertUseBitOp;
    function<int(uint_fast32_t)> countOnes = CountUseBitOp;

    string binary = int2binary(n);
    int streak = countOnes(n);

    return make_tuple(binary, streak);
}


int main() {
    string binary{};
    short streak{};

    tie(binary, streak) = solveUseBitOp();
    cout << streak << endl;
    return 0;
}
