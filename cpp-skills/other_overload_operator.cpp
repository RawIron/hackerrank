/**
 * Operator Overloading
 */

#include<iostream>

using namespace std;


class Complex {
public:
    int a;
    int b;

    Complex(int real, int imaginary) : a{real}, b{imaginary} {}
};

/**
 *  Overload operator + for the class complex
 *      + add two complex numbers as (a+ib) + (c+id) = (a+c) + i(b+d)
 */
Complex operator+(const Complex& lhs, const Complex& rhs) {
    return Complex(lhs.a + rhs.a, lhs.b + rhs.b);
}

/**
 *  Overload operator << for the class complex
 *      << print a complex number in the format "a+ib"
 */
ostream& operator<<(ostream& os, const Complex& value) {
    os << value.a << "+i" << value.b;
    return os;
}


/**
 * read("4 + i7") == {4, 7}
 */
 #include <regex>

pair<int,int> read(string s) {
    regex imaginary_number{ R"((\d+)\s*\+\s*i(\d+))" };
    smatch matches;
    regex_search(s, matches, imaginary_number);

    return { stoi(matches[1]), stoi(matches[2]) };
}

pair<int,int> read_c(string s) {
    size_t i = 0;

    int v1 = 0;
    while (s[i] != '+') { 
        v1 = v1*10 + s[i]-'0';
        ++i;
    }
    while (s[i] == ' ' || s[i] == '+' || s[i] == 'i') {
        ++i;
    }

    int v2 = 0;
    while (i < s.length()) {
        v2 = v2*10 + s[i]-'0';
        ++i;
    }

    return {v1, v2};
}


int main() {
    string s1, s2;
    cin >> s1;
    cin >> s2;

    auto [a, b] = read(s1);
    Complex x(a, b);
    auto [c, d] = read(s2);
    Complex y(c, d);
 
    Complex z = x + y;
    cout << z << endl;

    return EXIT_SUCCESS;
}
