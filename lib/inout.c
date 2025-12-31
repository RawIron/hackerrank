#include <stdio.h>
#include <stdlib.h>


int read_one() {
    int n = 0;
    scanf("%d", &n);
    return n;
}


int* read_many(const int n) {
    int * numbers = malloc(sizeof(int) * n);

    for (int i=0; i<n; ++i) {
        scanf("%d", &(numbers[i]));
    }

    return numbers;
}
