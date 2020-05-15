#include <iostream>
#include <vector>
#include <tuple>
#include <numeric>

using namespace std;


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
	const vector<int> testScores;  

    public:
   /**
	*   scores - An array of integers denoting the Person's test scores.
	*/
	explicit
	Student(string firstName, string lastName, int identification,
			vector<int> scores)
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


void solve(const string firstName, const string lastName, const int id,
		const vector<int> scores)
{
    Student* s = new Student(firstName, lastName, id, scores);
    s->show();
    cout << "Grade: " << s->calculate() << endl;
}


tuple<string,string,int> read_person() {
    string firstName{};
    string lastName{};
    int id{};
    cin >> firstName >> lastName >> id;

	return make_tuple(firstName, lastName, id);
}

vector<int> read_scores() {
    int numScores{};
    cin >> numScores;

    vector<int> scores{};
    for(int i = 0; i < numScores; i++) {
        int score{};
        cin >> score;
        scores.push_back(score);
    }

	return scores;
}


int main() {
    string firstName{};
    string lastName{};
    int id{};
	tie(firstName, lastName, id) = read_person();

    vector<int> scores{};
	scores = read_scores();

	solve(firstName, lastName, id, scores);

    return 0;
}
