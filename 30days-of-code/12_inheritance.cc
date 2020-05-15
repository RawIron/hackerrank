#include <iostream>
#include <limits>
#include <vector>
#include <tuple>
#include <numeric>

using namespace std;


namespace PersonIO {
enum Entries {FristName, LastName, Id};
using AsTuple = tuple<string, string, int>;
}
using TestScores = vector<int>;


class Person {
    protected:
	const string firstName;
	const string lastName;
	const int id;

    public:
   /**
	*   firstName - A string denoting the Person's first name.
	*   lastName - A string denoting the Person's last name.
	*   id - An integer denoting the Person's ID number.
	*/
	explicit
	Person(const string firstName, const string lastName, const int identification)
		: firstName{firstName}, lastName{lastName}, id{identification}
	{}

	void show() const {
		cout << "Name: " << lastName << ", " << firstName << "\nID: " << id << "\n"; 
	}
    
};


class Student : public Person {
    private:
	const TestScores testScores;  

    public:
   /**
	*   scores - An array of integers denoting the Person's test scores.
	*/
	explicit
	Student(string firstName, string lastName, int identification,
			TestScores scores)
		: Person{firstName, lastName, identification}, testScores{scores}
	{}

   /**
	*   Return: A character denoting the grade.
	*/
	string calculate() const {
		string grade{};
		double avgScore{};

		avgScore = accumulate(testScores.begin(), testScores.end(), 0.0)
				   / testScores.size();

		if (avgScore >= 90) { grade = "O" ; }
		else if (avgScore >= 80 && avgScore < 90) { grade = "E" ; }
		else if (avgScore >= 70 && avgScore < 80) { grade = "A" ; }
		else if (avgScore >= 55 && avgScore < 70) { grade = "P" ; }
		else if (avgScore >= 40 && avgScore < 55) { grade = "D" ; }
		else if (avgScore <  40) { grade = "T" ; }

		return grade;
	}
};


void solve(const PersonIO::AsTuple person, const vector<int> scores) {
    Student* s = new Student(get<0>(person), get<1>(person), get<2>(person), scores);
    s->show();
    cout << "Grade: " << s->calculate() << endl;
}


PersonIO::AsTuple readPerson() {
    string firstName{};
    string lastName{};
    int id{};
    cin >> firstName >> lastName >> id;
	cin.ignore(numeric_limits<streamsize>::max(), '\n');

	return make_tuple(firstName, lastName, id);
}

TestScores readScores() {
    int numScores{};
    cin >> numScores;

    TestScores scores{};
    for(int i = 0; i < numScores; i++) {
        int score{};
        cin >> score;
        scores.push_back(score);
    }
	cin.ignore(numeric_limits<streamsize>::max(), '\n');

	return scores;
}


int main() {
	PersonIO::AsTuple person{};
	person = readPerson();

    TestScores scores{};
	scores = readScores();

	solve(person, scores);

    return 0;
}
