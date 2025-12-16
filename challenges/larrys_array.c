#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#define DEBUG 0


/**
 *  move the smallest elem of triple
 *  to the front with rotations
 *  (a,b,c) -> swap (a) (b,c) == (b,c) (a)
 *             swap (a,b) (c) == (c) (a,b)
 *
 *  side-effect: inplace rotation
 *
 *  move_min_fst( [4, 9, 1] ) == [1, 4, 9]
 */
void move_min_fst(int* slice) {
    int min_pos = 0;
    for (int i=1; i<3; ++i) {
        if (*(slice + min_pos) > *(slice+i)) {
            min_pos = i;
        }
    }

    int temp[3];
    memcpy(temp, slice, 3 * sizeof(int));

    if (min_pos == 1) {
        // swap (a) (b,c) == (b,c) (a)
        memcpy(slice, &temp[1], 2 * sizeof(int));
        slice[2] = temp[0];
    }
    else if (min_pos == 2) {
        // swap (a,b) (c) == (c) (a,b)
        slice[0] = temp[2];
        memcpy(&slice[1], temp, 2 * sizeof(int));
    }
}


/**
 *  sort a given range shuffle
 *      indices [0..n-1]
 *      values  [1..n]
 *  only allowed operation is a rotation of any 3 elements
 *      (a,b,c) -> (b,c,a) -> (c,a,b) -> (a,b,c)
 *
 *  for any triple it is possible
 *      to bring the smallest elem to the front of the triple
 *
 *  like in bubble-sort move the min of the remaining range
 *      to the front using overlapping triples
 *      the min is _passed_ from triple to triple
 *  repeating the bubble-sort steps the given range
 *      can be sorted up to the last 2 elements
 *      n-2 elements are sorted
 *
 *  the whole range is sorted
 *      when the last triple (n-3, n-1) is sorted
 *      the first elem of that triple is the smallest
 *      sufficient to test (n-2, n-1) is sorted
 *
 *  Example of a first iteration
 *   4  2  5  1  3
 *  (2  5  4) 1  3
 *   2 (1  5  4) 3
 *   2  1 (3  5  4)
 *
 *  side-effect: inplace sort
 */
bool larrys_array(const int n, int* range_shuffle) {
    int from = 0;   // elems >= from
                    // are not yet sorted

    while (from < n-2) {
        for (int i=from; i<n-2; ++i) {
            move_min_fst( &range_shuffle[i] );
        }

        for (int i=from; i<n-2; ++i) {
            if (range_shuffle[i] == i+1) {
                from = i+1;
            }
            else { break; }
        }

#if DEBUG
        for (int i=0; i<n; ++i) {
            printf("%d ", range_shuffle[i]);
        }
        printf("\n");
 #endif
    }

    return (range_shuffle[n-2] == n-1);
}


int main() {
    FILE* fptr = fopen(getenv("OUTPUT_PATH"), "w");

    int tests = 0;
    scanf("%d", &tests);

    for (int i = 0; i < tests; ++i) {
        int n = 0;
        scanf("%d", &n);

        int* range_shuffle = malloc(n * sizeof(int));

        for (int i = 0; i < n; ++i) {
            int number;
            scanf("%d", &number);
            range_shuffle[i] = number;
        }

        const bool solvable = larrys_array(n, range_shuffle);

        free(range_shuffle);

        if (solvable) { fprintf(fptr, "YES\n"); }
        else { fprintf(fptr, "NO\n"); }
    }

    fclose(fptr);

    return 0;
}
