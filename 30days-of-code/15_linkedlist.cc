#include <iostream>
#include <vector>

using namespace std;	


template<class T>
class Node
{
	public:
	const T data;
	Node<T>* next;

	public:
	explicit Node(const T d)
		: data{d}, next{nullptr}
	{}
};


template<class T>
class MyList {
	public:
	explicit MyList() : tail{nullptr}
	{}

	Node<T>* insert(Node<T>* const head, const T data) {
		Node<T>* new_node = new Node<T>{data};

		if (head == nullptr) {
			tail = new_node;
			return new_node;
		}
		else {
			tail->next = new_node;
			tail = new_node;
			return head;
		}
	}

	void display(Node<T>* const head) const {
		Node<T>* start = head;
		while(start) {
		  cout << start->data << " ";
		  start = start->next;
		}
		cout << endl;
	}

	private:
	Node<T>* tail;
};


template<typename T>
vector<T> read_input() {
	int n{};
	cin >> n;
	const int number_lines{n};

	T item{};
	vector<T> data{};
	for (int i{number_lines}; i>0; --i) {
	    cin >> item;
	    data.push_back(item);
	}

	return data;
}

template<typename T>
void solve(const vector<T> data) {
	Node<T>* head{nullptr};
  	MyList<T> mylist{};

	for (auto item: data) {
	    head = mylist.insert(head, item);
	}

	mylist.display(head);
}


int main() {
	vector<int> data{};
	data = read_input<int>();

	solve(data);
	return 0;
}
