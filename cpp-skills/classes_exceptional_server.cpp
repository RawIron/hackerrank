#include <cmath>
#include <vector>
#include <iostream>
#include <stdexcept>
#include <cassert>

using namespace std;


class Server {
public:
    static int compute(long long a, long long b);
    static int getLoad();
private:
    inline static int load_ = 0;
};

int Server::compute(long long a, long long b) {
    load_ += 1;

    if (a < 0) { throw invalid_argument("A is negative"); }

    vector<int> v(a, 0);    // throws bad_alloc
    int _ = sqrt(-1);       // compiler will remove this line

    if (b == 0) throw 0;
    int real{ -1 };

    real = (a/b) * real;
    assert(abs(a/b) <= a);
    assert(real+a >= 0);

    int ans{ v.at(b) };     // throws out_of_range
    assert(ans == 0);

    return real + a - b*ans;    // throws overflow_error
}

int Server::getLoad() { return load_; }


void runTests() {
    assert(Server::compute(25581, 3661) == 25575);
    
    try { Server::compute(3850, 921492); }
    catch (out_of_range e) { cout << e.what() << endl; }  
}


void solve() {
    int tests_total;
    cin >> tests_total;
    
    while(tests_total--) {
        long long a, b;
        cin >> a >> b;
        
        try {
            int value{ Server::compute(a, b) };
            cout << value << endl;
        }
        
        catch (int _) {     // Server::compute
            cout << "Other Exception" << endl;;
        }
        catch (invalid_argument e) {    // Server::compute
           cout << "Exception: " << e.what() << endl;
        }
        catch (overflow_error e) {    // Server::compute long + long
            cout << "Exception: " << e.what() << endl;
        }
        catch (bad_array_new_length e) {    // vector constructor
            cout << "Exception: " << e.what() << endl;
        }
        catch (bad_alloc e) {       // vector constructor
           cout << "Not enough memory" << endl;
        }
        catch (out_of_range e) {    // vector at
            cout << "Exception: " << e.what() << endl;
        }
        catch (exception e) {
            cout << "Exception: " << e.what() << endl;
        }
        catch (...) {
            cout << "Other Exception" << endl;
        }
    }
    cout << Server::getLoad() << endl;  
}


int main() {
    //runTests();
    solve();

    return EXIT_SUCCESS;
}

