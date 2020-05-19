#include <cmath>
#include <cstdio>
#include <iostream>
#include <algorithm>
#include <string>

using namespace std;


class AdvancedArithmetic {
protected:
    AdvancedArithmetic() {}

public:
    virtual int divisorSum(const int n) = 0;
    virtual ~AdvancedArithmetic() {}
};


class Calculator : public AdvancedArithmetic {
public:
    virtual int divisorSum(const int n) override {
        if (n == 1) {
            return 1;
        }

        int begin{ n / 2 };
        int step{ 1 };

        bool nIsEven{ (n % 2 == 0) };
        if (not nIsEven) {
            step = 2;
            bool nDiv2IsEven{ ((n / 2) % 2 == 0) };
            if ( nDiv2IsEven ) {
                begin -= 1;
            }
        }

        int divSum { n + 1 };
        for (int i{begin}; i>1; i-=step) {
            if (n % i == 0) {
                divSum += i;
            }
        }

        return divSum;
    }
};


int main() {
    int n;
    cin >> n;

    AdvancedArithmetic *myCalculator = new Calculator(); 
    int sum = myCalculator->divisorSum(n);

    cout << "I implemented: AdvancedArithmetic\n" << sum;
    return 0;
}
