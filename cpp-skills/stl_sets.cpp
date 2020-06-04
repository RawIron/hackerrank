#include <iostream>
#include <set>
#include <vector>
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
    int value;
};


void readActions(vector<Action>& actions) {
    int n{};
    cin >> n;

    int actionType{};
    int value{};
    for (int i{0}; i<n; ++i) {
        cin >> actionType;
        cin >> value;
        Action action{ static_cast<ActionType>(actionType), value };
        actions.push_back(action);
    }
}


void show(const bool yesno) {
    if (yesno) { cout << "Yes" << endl; }
    else { cout << "No" << endl; }
}

void show() {}


function<void(set<int>&)> route(const Action& action) {
    function<void(set<int>&)> doAction{};

    switch (action.type) {
    case ActionType::insert:
        doAction = [action](set<int>& uniques) { uniques.insert(action.value); };
        break;
    case ActionType::erase:
        doAction = [action](set<int>& uniques) { uniques.erase(action.value); };
        break;
    case ActionType::find:
        doAction = [action](set<int>& uniques) {
            set<int>::iterator it = uniques.find(action.value);
            const bool found = (it != uniques.end());
            show(found);
        };
        break;
    }

    return doAction;
}


int main() {
    vector<Action> actions{};
    readActions(actions);

    set<int> uniqueValues{};
    for (const auto& action : actions) {
        route(action)(uniqueValues);
    }

    return 0;
}
