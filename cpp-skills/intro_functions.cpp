#include <iostream>

using namespace std;

int maxOfFour(const int a, const int b, const int c, const int d) {
    return max(max(a,b),max(c,d));
}

int main() {
    int a{}, b{}, c{}, d{};
    scanf("%d %d %d %d", &a, &b, &c, &d);

    int maxIs{ maxOfFour(a, b, c, d) };
    cout << maxIs << endl;

    return 0;
}
