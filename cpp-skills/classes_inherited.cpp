#include <iostream>
#include <iterator>
#include <algorithm>
#include <string>
#include <sstream>
#include <vector>
#include <exception>

using namespace std;


class BadLengthException : public exception {
    private:
    const int n_;
    string message_;

    public:
    explicit BadLengthException(const int n) : n_{n}, message_{to_string(n_)} {}
    
    const char* what() const throw() {
        return message_.c_str();
    }
};


bool checkUsername(string username) {
	bool isValid{true};
	size_t n{ username.length() };

	if (n < 5) {
		throw BadLengthException(n);
	}
	for (size_t i{0}; i < n-1; i++) {
		if (username[i] == 'w' && username[i+1] == 'w') {
			isValid = false;
		}
	}

	return isValid;
}

void solve(const string username) {
    try {
        const bool isValid = checkUsername(username);
        if (isValid) { cout << "Valid" << '\n'; }
        else { cout << "Invalid" << '\n'; }
    }
    catch (BadLengthException &e) {
        cout << "Too short: " << e.what() << '\n';
    }
}


vector<string> readUsers() {
	int n{};
    cin >> n;

    vector<string> users{};
    copy_if(
        istream_iterator<string>(cin),
        istream_iterator<string>(),
        back_inserter(users),
        [count=n] (string) mutable { return count && count--; }
    );
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    return users;
}

int main() {
	for (string& username : readUsers()) {
        solve(username);
	}

    // the first call to readUsers empties cin
    // no user will be printed
    for (string& user: readUsers()) {
        cout << user << endl;
    }

	return 0;
}
