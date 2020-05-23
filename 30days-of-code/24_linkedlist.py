class Node:
    def __init__(self, data):
        self.data = data
        self.next = None

class LinkedList:
    def insert(self, head, data):
        p = Node(data)
        if head is None:
            head = p
        elif head.next is None:
            head.next = p
        else:
            start = head
            while start.next is not None:
                start = start.next
            start.next = p
        return head

    def display(self, head):
        current = head
        while current:
            print(current.data, end=' ')
            current = current.next
        print()

    def remove_duplicates(self, head):
        if head is None:
            return head

        current = head
        streak = head
        while current.next is not None:
            current = current.next
            if current.data == streak.data:
                streak.next = current.next
            else:
                streak = current

        return head


def generate_input():
    n_lines = int(input())
    for _ in range(n_lines):
        yield int(input())

def read_input():
    n_lines = int(input())
    items = []
    for _ in range(n_lines):
        items.append(int(input()))
    return items


if __name__ == "__main__":
    read_data = generate_input

    mylist = LinkedList()
    head = None
    for data in read_data():
        head = mylist.insert(head, data)
    head = mylist.remove_duplicates(head)

    mylist.display(head)
