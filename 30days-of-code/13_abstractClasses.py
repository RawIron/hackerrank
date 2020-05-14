from abc import ABCMeta, abstractmethod


class Book(object, metaclass=ABCMeta):
    def __init__(self, title, author):
        self.title=title
        self.author=author   

    @abstractmethod
    def display(): pass


class MyBook(Book):
    def __init__(self, title, author, price):
        super(MyBook, self).__init__(title, author)
        self.price = price

    def display(self):
        print(f"Title: {self.title}")
        print(f"Author: {self.author}")
        print(f"Price: {self.price}")


def solve(title, author, price):
    new_novel = MyBook(title,author,price)
    new_novel.display()


if __name__ == "__main__":
    title=input()
    author=input()
    price=int(input())

    solve(title, author, price)
