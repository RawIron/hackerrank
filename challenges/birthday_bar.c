#include <stdio.h>
#include <stdlib.h>


/**
 *  count all the slices in the array
 *  where it's sum == sum_target
 *
 *  does not calc the sum for every slice
 *  instead
 *      calc the sum for the first slice
 *      do a rolling sum for all other slices
 *
 *  birthday(6, [2,3,2,7,1,4], 5, 2) == 3
 */
int birthday(const size_t s_size, const int* s, const int sum_target, const int length) {
    int sum = 0;
    int count = 0;

    for (size_t i = 0; i < length; ++i) {
        sum += s[i];
    }
    if (sum == sum_target) {
        ++count;
    }

    for (size_t i = 1; i < s_size - (length-1); ++i) {
        sum = sum - s[i-1] + s[i+(length-1)];
        if (sum == sum_target) {
            ++count;
        }
    }

    return count;
}


int main() {
    int squares = 0;
    scanf("%d", &squares);
    
    int * numbers = malloc(sizeof(int) * squares);
    
    for (int i=0; i<squares; ++i) {
        scanf("%d", &(numbers[i]));
    }

    int day = 0;
    scanf("%d", &day);
    int month = 0;
    scanf("%d", &month);

    const int result = birthday(squares, numbers, day, month);

    free(numbers);

    FILE* fptr = fopen(getenv("OUTPUT_PATH"), "w");
    fprintf(fptr, "%d\n", result);
    fclose(fptr);

    return 0;
}
