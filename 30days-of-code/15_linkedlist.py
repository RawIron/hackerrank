class Node:
    def __init__(self,data):
        self.data = data
        self.next = None

class Solution:
    def dump(self, head):
        current = head
        while current:
            yield current.data
            current = current.next

    def insert(self, head, data):
        newnode = Node(data)
        if head is None:
            self.tail = newnode
            return newnode
        else:
            self.tail.next = newnode
            self.tail = newnode
            return head


def solve(data):
    mylist = Solution()
    head = None
    for item in data:
        head = mylist.insert(head, item)    

    return mylist.dump(head)


def generate_numbers():
    N = int(input())
    for _ in range(N):
        yield int(input())


def read_numbers():
    N = int(input())
    numbers = []
    for _ in range(N):
        numbers.append(int(input()))
    return numbers


if __name__ == "__main__":
    read_input = generate_numbers

    dumped = solve( read_input() )

    for d in dumped:
        print(d, end=' ')
    print()
