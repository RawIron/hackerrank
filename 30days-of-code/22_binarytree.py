class Node:
    def __init__(self, value):
        self.right = self.left = None
        self.data = value

class Solution:
    def insert(self, root, value):
        if root is None:
            return Node(value)

        if value <= root.data:
            cur = self.insert(root.left, value)
            root.left = cur
        else:
            cur = self.insert(root.right, value)
            root.right = cur
        return root

    def get_height(self, root_node):
        def longest_path(node, height):
            left_height = right_height = 0
            if node.left is not None:
                left_height = longest_path(node.left, height)
                left_height += 1
            if node.right is not None:
                right_height = longest_path(node.right, height)
                right_height += 1
            cur_height = max(left_height, right_height) + height
            return cur_height

        if root_node is None:
            return 0

        return longest_path(root_node, 0)

def read_input():
    n_lines = int(input())
    values = []
    for _ in range(n_lines):
        values.append(int(input()))
    return values


if __name__ == "__main__":
    # this is global namespace !!
    data = read_input()

    myTree = Solution()
    tree_root = None
    for elem in data:
        tree_root = myTree.insert(tree_root, elem)
    tree_height = myTree.get_height(tree_root)

    print(tree_height)
