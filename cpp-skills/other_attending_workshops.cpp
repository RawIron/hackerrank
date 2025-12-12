#include<bits/stdc++.h>

using namespace std;


namespace Solution_cpp {

}


namespace Solution_c {
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


struct Workshop {
    int start_time;
    int end_time;
    int duration;
};

struct Workshops {
    int total;
    Workshop* available;
};


void show(const Workshop* w) {
     cout << w->start_time << " " << w->end_time << endl;
}

void show(const Workshops* ws) {
    for (size_t i{0}; i < ws->total; ++i) {
        show( &(ws->available[i]) );
    }
}


int comp (const void * elem1, const void * elem2) {
    int a = ((Workshop*)elem1)->start_time;
    int b = ((Workshop*)elem2)->start_time;
    if (a > b) return  1;
    if (a < b) return -1;
    return 0;
}

/**
 *  zip (start_times, durations, calculated column end_time)
 *  sort by start_time
 */
Workshops * initialize(const int start_time[], const int duration[], const int n) {
    Workshop * available = new Workshop[n];

    for (size_t i{0}; i < n; ++i) {
        Workshop w;
        w.start_time = start_time[i];
        w.end_time = start_time[i] + duration[i];
        w.duration = duration[i];
        available[i] = w;
    }

    qsort (available, n, sizeof(*available), comp);

    Workshops * workshops = new Workshops();
    workshops->total = n;
    workshops->available = available;
    return workshops;
}


/**
 * workshops with the same start_time
 * pick the one with the min duration
 *
 * side-effect:
 *  moves position in Workshops array forward
 */
Workshop* dedup(Workshop** ptr, const Workshop* end) {
    Workshop* choosen = *ptr;
    ++(*ptr);

    while ( (*ptr) < end && (*ptr)->start_time == choosen->start_time ) {
        if ((*ptr)->duration < choosen->duration) {
            choosen = (*ptr);
        }
        ++(*ptr);
    }

    return choosen;
}

/**
 *  the two workshops overlap
 *  keep the one which ends earlier
 */
Workshop* pick(Workshop* fst, Workshop* snd) {
    if (fst->end_time < snd->end_time) {
        return fst;
    }
    else {
        return snd;
    }
}

/**
 *  sign up in case there is no overlap
 *  
 *  if there is an overlap
 *      choose the workshop which ends earlier
 *      because this is the best option given
 *      the goal to sign up for as many workshops
 *      as possible
 */
int signUpMax(const Workshops* wshops) {
    int signedUp = 0;

    Workshop* end = wshops->available + wshops->total;
    Workshop* pos = wshops->available;

    Workshop* fst = dedup(&pos, end);
    ++signedUp;

    while (pos < end) {
        Workshop* snd = dedup( &pos, end);

        if (fst->end_time <= snd->start_time) {
            // no overlap
            // sign up for second
            fst = snd;
            ++signedUp;
        }
        else {
            // overlap
            // eliminate one
            fst = pick(fst, snd);
        }
        //show(fst);
        //cout << signedUp << endl;
    }
    //cout << endl;
    return signedUp;
}


void run_tests() {
    Workshop* w;
    Workshops ws;
    int have;

    //  +--+  +---+ +--+
    w = new Workshop[3] { {0,2,2}, {3,4,1}, {5,5,0} };
    ws = { 3, w };
    assert( signUpMax(&ws) == 3 );
    free(w);

    //  +--+
    //     +---+
    //     +-----+
    w = new Workshop[3] { {0,2,2}, {2,3,1}, {2,4,2} };
    ws = { 3, w };
    assert( signUpMax(&ws) == 2 );
    free(w);

    //  +--+
    //       +---+
    //         +-----+
    w = new Workshop[3] { {0,2,2}, {3,5,2}, {4,6,2} };
    ws = { 3, w };
    assert( signUpMax(&ws) == 2 );
    free(w);

    //  +--+
    //     +---+
    //        +-----+
    w = new Workshop[3] { {0,2,2}, {2,4,2}, {3,5,2} };
    ws = { 3, w };
    assert( signUpMax(&ws) == 2 );
    free(w);

    //  +------------+
    //     +-----+
    //             +----+
    w = new Workshop[3] { {0,6,6}, {2,3,1}, {4,7,3} };
    ws = { 3, w };
    assert( signUpMax(&ws) == 2 );
    free(w);

    //  +--+
    //  +---+
    //  +-----+
    w = new Workshop[3] { {0,2,2}, {0,3,3}, {0,4,4} };
    ws = { 3, w };
    assert( signUpMax(&ws) == 1 );
    free(w);

    //  +------+
    //     +-----+
    //       +----+
    w = new Workshop[3] { {0,6,6}, {3,7,4}, {5,8,3} };
    ws = { 3, w };
    have = signUpMax(&ws);
    assert( have == 1 );
    free(w);

    //  +--------------+
    //      +-----+
    //          +----+
    w = new Workshop[3] { {0,6,6}, {1,3,2}, {2,5,3} };
    ws = { 3, w };
    assert( signUpMax(&ws) == 1 );
    free(w);

    //  +-+-+-+-+-+
    w = new Workshop[5] { {1,2,1}, {2,3,1}, {3,4,1}, {4,5,1}, {5,6,1} };
    ws = { 5, w };
    have = signUpMax(&ws);
    assert( have == 5 );
    free(w);
}


int* read(size_t n) {
    int* numbers = new int[n];
    for(size_t i=0; i < n; ++i) {
        cin >> numbers[i];
    }
    return numbers;
}


void solve() {
    int totalWorkshops;
    cin >> totalWorkshops;

    int* start_times = read(totalWorkshops);
    int* durations = read(totalWorkshops);

    Workshops * workshops = initialize(start_times, durations, totalWorkshops);
    cout << signUpMax( workshops ) << endl;

    free(start_times);
    free(durations);
    free(workshops->available);
    free(workshops);
}

}


int main() {

    Solution_c::solve();
    //Solution_c::run_tests();

    return EXIT_SUCCESS;
}
