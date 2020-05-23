#include <iostream>
#include <vector>

using namespace std;    


class Node {
    public:
    const int data;
    Node* left;
    Node* right;

    public:
    explicit Node(const int d) : data{d}, left{nullptr}, right{nullptr}
    {}
};


class BinaryTree {
    public:
    Node* insert(Node* root, const int data) {
        if (not root) {
            return new Node(data);
        }

        Node* cur{};
        if (data <= root->data) {
            cur = insert(root->left, data);
            root->left = cur;
        }
        else {
            cur = insert(root->right, data);
            root->right = cur;
       }

       return root;
    }

    int getHeight(const Node* const root) const {
        if (not root) {
            return 0;
        }
        return walk(root, 0);
    }

    vector<vector<Node*>> listBreadthFirst(Node* root) const {
        if (not root) {
            return vector<vector<Node*>>{};
        }

        vector<vector<Node*>> levels{};
        vector<Node*> nextLevel{};
        vector<Node*> currentLevel{root};

        while (not currentLevel.empty()) {
            levels.push_back(currentLevel);
            vector<Node*>{}.swap(nextLevel);
            for (auto node : currentLevel) {
                if (node->left) {
                    nextLevel.push_back(node->left);
                }
                if (node->right) {
                    nextLevel.push_back(node->right);
                }
            }
            currentLevel = nextLevel;
        }

        return levels;
    }

    private:
    int walk(const Node* const node, const int height) const {
        int leftHeight{0}, rightHeight{0};
        if (node->left) {
            leftHeight = walk(node->left, height);
            ++leftHeight;
        }
        if (node->right) {
            rightHeight = walk(node->right, height);
            ++rightHeight;
        }
        return max(leftHeight, rightHeight) + height;
    }

};


vector<int> readMany() {
    int t{};
    cin >> t;

    int value{};
    vector<int> values{};
    while(t-- > 0){
        cin >> value;
        values.push_back(value);
    }
    return values;
}


int main() {
    vector<int> items = readMany();

    BinaryTree myTree{};
    Node* root{nullptr};
    for (auto item : items) {
        root = myTree.insert(root, item);
    }

    int height = myTree.getHeight(root);
    cout << height << endl;

    vector<vector<Node*>> treeLevels = myTree.listBreadthFirst(root);
    for (auto level : treeLevels) {
        for (auto node : level) {
            cout << node->data << " ";
        }
    }
    cout << endl;

    return 0;
}
