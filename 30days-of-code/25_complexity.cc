#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;


template<typename T>
vector<T> readManyFromSingleLine() {
    size_t N{};
    cin >> N;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    vector<T> in_list{};
    for (size_t i{0}; i<N; ++i) {
        T element{};
        cin >> element;
        
        in_list.push_back(element);
    }
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return in_list;
}

bool isPrime(const long number) {
    if (number == 1 ) {
        return false;
    }
    if (number == 2) {
        return true;
    }

    const bool isEven{ number % 2 == 0 };
    const long lastDigit{ number % 10 };
    const bool isDivisibleBy5{ number > 10 and lastDigit == 5 };
    const bool notPrimeByRule{ isEven or isDivisibleBy5 };

    if (notPrimeByRule) {
        return false;
    }

    for (long divisor{3}; divisor < sqrt(number)+1; divisor += 2) {
        if (number % divisor == 0) {
            return false;
        }
    }

    return true;
}


int main() {
    const vector<long> numbers{ readManyFromSingleLine<long>() };

    for (auto number : numbers) {
        if (isPrime(number)) {
            cout << "Prime" << endl;
        }
        else {
            cout << "Not prime" << endl;
        }
    }
       
    return 0;
}
