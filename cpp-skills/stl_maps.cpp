#include <iostream>
#include <map>
#include <vector>
#include <tuple>
#include <algorithm>
#include <functional>

using namespace std;


enum class ActionType {
    insert = 1,
    erase,
    find
};

struct Action {
    ActionType type;
    string name;
    int marks;
};

using Students = map<string,int>;


void readActions(vector<Action>& actions) {
    int n{};
    cin >> n;

    for (int i{0}; i<n; ++i) {
        int actionType{};
        string name{};
        int marks{0};
        cin >> actionType;
        cin >> name;
        if (actionType == 1) {
            cin >> marks;
        }
        Action action{ static_cast<ActionType>(actionType), name, marks };
        actions.push_back(action);
    }
}


void show(const bool yesno) {
    if (yesno) { cout << "Yes" << endl; }
    else { cout << "No" << endl; }
}

void show(const int number) {
    cout << number << endl;
}

void show() {}


function<void(Students&)> route(const Action& action) {
    function<void(Students&)> doAction{};

    switch (action.type) {
    case ActionType::insert:
        doAction = [action](Students& students) {
            pair<Students::iterator, bool> result;
            result = students.insert(make_pair(action.name, action.marks));
            bool isInMap{ result.second == false };
            if (isInMap) {
                (result.first)->second += action.marks;
            }};
        break;
    case ActionType::erase:
        doAction = [action](Students& students) { students.erase(action.name); };
        break;
    case ActionType::find:
        doAction = [action](Students& students) {
            map<string,int>::iterator it = students.find(action.name);
            const bool found = (it != students.end());
            if (found) { show(it->second); }
            else { show(0); }
        };
        break;
    }

    return doAction;
}


int main() {
    vector<Action> actions{};
    readActions(actions);

    Students students{};
    for (const auto& action : actions) {
        route(action)(students);
    }

    return 0;
}
