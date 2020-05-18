#include <iostream>
#include <queue>
#include <stack>

using namespace std;


class Solution {
    private:
    queue<char> word_queue;
    stack<char> word_stack;

    public:
    Solution() : word_queue{}, word_stack{}
    {}

    void push(const char letter) {
        word_stack.push(letter);
    }

    char pop() {
        char letter{};
        letter = word_stack.top();
        word_stack.pop();
        return letter;
    }

    void enqueue(const char letter) {
        word_queue.push(letter);
    }

    char dequeue() {
        char letter{};
        letter = word_queue.front();
        word_queue.pop();
        return letter;
    }
};


bool solve(const string word) {
    Solution solution{};

    for (size_t i = 0; i < word.length(); i++) {
        solution.push(word[i]);
        solution.enqueue(word[i]);
    }

    bool isPalindrome = true;
    for (size_t i = 0; i < word.length() / 2; i++) {
        if (solution.pop() != solution.dequeue()) {
            isPalindrome = false;
            break;
        }
    }

    return isPalindrome;
}

void show(const string word, const bool isPalindrome) {
    if (isPalindrome) {
        cout << "The word, " << word << ", is a palindrome." << endl;
    }
    else {
        cout << "The word, " << word << ", is not a palindrome." << endl;
    }
}


string readOne() {
    string word{};
    getline(cin, word);
    return word;
}


int main() {
    // read_one | solve | show
    string word{ readOne() };
    bool isPalindrome = solve(word);
    show(word, isPalindrome);
    
    return 0;
}
