#!/bin/python3

import os

class SinglyLinkedListNode:
    def __init__(self, node_data):
        self.data = node_data
        self.next = None

class SinglyLinkedList:
    def __init__(self):
        self.head = None
        self.tail = None

    def insert_node(self, node_data):
        node = SinglyLinkedListNode(node_data)
        if not self.head:
            self.head = node
        else:
            self.tail.next = node
        self.tail = node

def print_singly_linked_list(node, sep, fptr):
    while node:
        fptr.write(str(node.data))
        node = node.next
        if node:
            fptr.write(sep)


def findMergeNode(head1, head2):
    heads = set()
    while head1 is not None or head2 is not None:
        if head1 is not None:
            if id(head1) not in heads:
                heads.add(id(head1))
            else:
                return head1.data
            head1 = head1.next
        if head2 is not None:
            if id(head2) not in heads:
                heads.add(id(head2))
            else:
                return head2.data
            head2 = head2.next
    return None


def printLinkedList(head):
    idx = 0
    while head is not None:
        print(idx, head.data, id(head))
        idx += 1
        head = head.next


def read_input():
    index = int(input())

    llist1 = SinglyLinkedList()
    llist1_count = int(input())
    for _ in range(llist1_count):
        llist1_item = int(input())
        llist1.insert_node(llist1_item)

    llist2 = SinglyLinkedList()
    llist2_count = int(input())
    for _ in range(llist2_count):
        llist2_item = int(input())
        llist2.insert_node(llist2_item)

    ptr1 = llist1.head;
    ptr2 = llist2.head;
    for i in range(llist1_count):
        if i < index:
            ptr1 = ptr1.next
    for i in range(llist2_count):
        if i != llist2_count-1:
            ptr2 = ptr2.next
    ptr2.next = ptr1

    return llist1, llist2


if __name__ == '__main__':
    fptr = open(os.environ['OUTPUT_PATH'], 'w')

    tests = int(input())
    for _ in range(tests):
        llist1, llist2 = read_input()
        result = findMergeNode(llist1.head, llist2.head)
        fptr.write(str(result) + '\n')

    fptr.close()
