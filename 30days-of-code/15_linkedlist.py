class Node:
    def __init__(self,data):
        self.data = data
        self.next = None 

class Solution: 
    def display(self,head):
        current = head
        while current:
            print(current.data,end=' ')
            current = current.next
        print()

    def insert(self,head,data):
        newnode = Node(data)
        if head is None:
            self.tail = newnode
            return newnode
        else:
            self.tail.next = newnode
            self.tail = newnode
            return head


def generate_numbers():
    N = int(input())
    for _ in range(N):
        yield int(input())

def read_numbers():
    T = int(input())
    numbers = []
    for _ in range(T):
        numbers.append(int(input()))
    return numbers


def solve():
    read_input = generate_numbers
    data = read_input()

    mylist = Solution()
    head = None
    for item in data:
        head = mylist.insert(head,item)    

    mylist.display(head); 	  


if __name__ == "__main__":
    solve()
