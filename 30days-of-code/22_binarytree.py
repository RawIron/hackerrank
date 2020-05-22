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


    def level_order(self, root):
        if root is None: return []

        levels = []
        next_level = []
        current_level = [root]
        while current_level:
            levels.append(current_level)
            next_level = []
            for node in current_level:
                if node.left is not None: next_level.append(node.left)
                if node.right is not None: next_level.append(node.right)
            current_level = next_level

        return levels


def read_input():
    n_lines = int(input())
    values = []
    for _ in range(n_lines):
        values.append(int(input()))
    return values


if __name__ == "__main__":
    # this is global namespace !!
    data = read_input()

    my_tree = Solution()
    tree_root = None
    for elem in data:
        tree_root = my_tree.insert(tree_root, elem)

    tree_height = my_tree.get_height(tree_root)
    print(tree_height)

    tree_levels = my_tree.level_order(tree_root)
    print(' '.join(map(str, (node.data for alevel in tree_levels for node in alevel))))
