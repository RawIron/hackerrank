#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>


/*
 * fives: 6, threes: 0  -> 555555
 * fives: 3, threes: 10 -> 5553333333333
 * fives: 9, threes: 5  -> 55555555533333
 *
 */
char *expand(const int fives, const int threes) {
    char *expanded = NULL;

    if (threes % 5 == 0) {
        expanded = malloc((fives + threes + 1) * sizeof(char));
        if (!expanded) return NULL;
        
        int idx = 0;
        for (int i = 0; i < fives; ++i, ++idx) {
            expanded[idx] = '5';
        }
        for (int i = 0; i < threes; ++i, ++idx) {
            expanded[idx] = '3';
        }
        expanded[idx] = '\0';
    }
    else {
        expanded = malloc((2 + 1) * sizeof(char));
        if (!expanded) return NULL;

        strcpy(expanded, "-1");
    }

    return expanded;
}


typedef struct {
  int fives;
  int threes;
} decent_simple;

/*
 * 6  ->  fives:  6, threes: 0
 * 7  ->  -1
 * 12 ->  fives: 12, threes: 0
 * 13 ->  fives: 3,  threes: 10
 * 14 ->  fives: 9,  threes: 5
 * 15 ->  fives: 15, threes: 0
 * 16 ->  fives: 6,  threes: 10
 * 17 ->  fives: 12, threes: 5
 * ..
 */
decent_simple calc_decent_number(const int digits) {
    int fives = (digits / 3) * 3; // digits-2 <= fives <= digits
                                  // 0 <= digits - fives < 3
    int threes = digits % 3;      // 0 <= threes < 3

    while (threes % 5 != 0 && fives > 0) {
        fives -= 3;
        threes = digits - fives;  // = 1 + 3*n
                                  //  (1 + 3*3) % 5 == 0
                                  // = 2 + 3*n
                                  //  (2 + 3*1) % 5 == 0
    }

    decent_simple number = { fives, threes };
    return number;
}


int main(void) {
    int tests_total = 0;
    scanf("%d", &tests_total);

    for (int i=0; i < tests_total; ++i) {
        int digits = 0;
        scanf("%d", &digits);

        decent_simple number = calc_decent_number(digits);
        char *result = expand(number.fives, number.threes);
        if (result) {
            printf("%s\n", result);
            free(result);
        }
    }

    return 0;
}
