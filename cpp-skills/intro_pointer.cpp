#include <stdio.h>
#include <cmath>


void update(int *a, int *b) {
    int keepA{ (*a) };
    (*a) = (*a) + (*b);
    (*b) = abs(keepA - (*b));
}

int main() {
    int a{}, b{};
    int *pa{ &a };
    int *pb{ &b };

    scanf("%d %d", &a, &b);
    update(pa, pb);
    printf("%d\n%d\n", a, b);

    return 0;
}
