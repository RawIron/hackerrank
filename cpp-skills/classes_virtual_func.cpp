#include <cstdio>
#include <iostream>
#include <vector>
#include <memory>

using namespace std;


class Person {
public:
    virtual ~Person() = default;
    virtual void getdata() = 0;
    virtual void putdata() const = 0;
protected:
    string name_;
    int age_;
};


class Professor : public Person {
public:
    void getdata() override;
    void putdata() const override;
private:
    int publications_;
    int cur_id_;
    inline static int id_ = 1;
};

void Professor::getdata() {
    cin >> name_ >> age_ >> publications_;
    cur_id_ = Professor::id_;
    ++Professor::id_;
}

void Professor::putdata() const {
    cout << name_ << " " << age_ << " " << publications_ << " " << cur_id_ << " " << endl;
}
    

class Student : public Person {
public:
    void getdata() override;
    void putdata() const override;
private:
    static const int MaxMarks = 6; 
    int marks_[MaxMarks];
    int cur_id_;
    // C++17 inline static member
    // define and initialise directly inside the class definition
    inline static int id_ = 1;
};

void Student::getdata() {
    cin >> name_ >> age_;
    for (int i{0}; i<MaxMarks; ++i) {
        cin >> marks_[i];
    }
    cur_id_ = Student::id_;
    ++Student::id_;
}

void Student::putdata() const {
    int sum{0};
    cout << name_ << " " << age_ << " ";
    for (int i{0}; i<MaxMarks; ++i) {
        sum += marks_[i];
    }
    cout << sum << " ";
    cout << cur_id_ << endl;
}


int main(void) {
    enum class ClassType {ProfessorT=1, StudentT};

    int input_len{};
    cin >> input_len;
    
    vector<unique_ptr<Person>> people;

    for (int i{0}; i<input_len; ++i) {
        int raw{};
        cin >> raw;
    
        ClassType class_type{ static_cast<ClassType>(raw) };
        switch (class_type) {
        case ClassType::ProfessorT: {
            people.push_back(make_unique<Professor>());
            break;
            }         
        case ClassType::StudentT: {
            people.push_back(make_unique<Student>());
            break;
            }
        }
        people[i]->getdata();
        people[i]->putdata();
    }
  
    return EXIT_SUCCESS;
}

