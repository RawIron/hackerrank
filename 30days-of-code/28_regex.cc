#include <bits/stdc++.h>

using namespace std;

using Emails = vector<pair<string, string>>;


template <typename T>
pair<T,T> readPair() {
    T fst{}, snd{};
    cin >> fst;
    cin >> snd;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');
    return make_pair(fst, snd);
}

Emails readInput() {
    size_t n{};
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    Emails emails{};
    for (size_t i{0}; i < n; ++i) {
        pair<string, string> email = readPair<string>();
        emails.push_back(email);
    }

    return emails;
}


vector<string> filterHasGmail(Emails emails) {
    vector<string> gmailAddr{};

    regex gmail_pattern(R"([a-z]+(\.[a-z]+)?@gmail\.com$)");
    string firstName{}, emailAddr{};
    for (const auto& email : emails) {
        tie(firstName, emailAddr) = email;
        if (regex_match(emailAddr, gmail_pattern)) {
            gmailAddr.push_back(firstName);
        }
    }
    
    return gmailAddr;
}


int main() {
    // read | filter | show

    Emails emails{ readInput() };

    vector<string> gmailUsers = filterHasGmail(emails);
    sort(gmailUsers.begin(), gmailUsers.end());

    for (const string& firstName : gmailUsers) {
        cout << firstName << endl;
    }

    return 0;
}
