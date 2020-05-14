class Person:
    #   first_name - A string denoting the Person's first name.
    #   last_name - A string denoting the Person's last name.
    #   id - An integer denoting the Person's ID number.
    def __init__(self, first_name, last_name, id_number):
        self.first_name = first_name
        self.last_name = last_name
        self.id_number = id_number

    def show(self):
        print("Name:", self.last_name + ",", self.first_name)
        print("ID:", self.id_number)


import types
import functools

class Student(Person):
    #   scores - An array of integers denoting the Person's test scores.
    def __init__(self, first, last, number, scores):
        super(Student, self).__init__(first, last, number)
        self.scores = scores

    #   Return: A character denoting the grade.
    def calculate(self):
        grade = None
        if not isinstance(self.scores, types.GeneratorType):
            avg_score = sum(self.scores) / float(len(self.scores))
        else:
            # object of type 'generator' has no len()
            size, total = functools.reduce(lambda acc, x: (acc[0]+1, acc[1]+x), self.scores, (0,0))
            avg_score = total / float(size)

        if   90 <= avg_score <= 100: grade = "O"
        elif 80 <= avg_score < 90: grade = "E"
        elif 70 <= avg_score < 80: grade = "A"
        elif 55 <= avg_score < 70: grade = "P"
        elif 40 <= avg_score < 55: grade = "D"
        elif avg_score < 40: grade = "T"

        return grade


def solve(first_name, last_name, idNum, scores):
    student = Student(first_name, last_name, idNum, scores)
    student.show()
    print("Grade:", student.calculate())


if __name__ == "__main__":
    (in_first_name, in_last_name, in_id_num) = input().split()

    _ = int(input())
    #in_scores = list(map(int, input().split()))
    in_scores = (int(score) for score in input().split())

    solve(in_first_name, in_last_name, in_id_num, in_scores)
