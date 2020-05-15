#include <string>
#include <iostream>
#include <stdexcept>


using namespace std;


int main() {
    string word{};
    cin >> word;

    int_fast32_t number{};
    try {
        number = stoi(word);
        cout << number << endl;
    }
    catch (const invalid_argument& ia) {
        cout << "Bad String" << endl;
    }

    return 0;
}
