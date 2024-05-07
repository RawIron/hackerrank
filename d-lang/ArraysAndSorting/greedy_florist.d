import std.stdio : writeln;
import std.cstream : din;
import std.conv : to;
import std.format: formattedRead;
import std.algorithm : splitter, sort, map;
import std.array;

/*
    cost of an item increases depending on
    how many items that person previously bought

    one person
    [2,4,1] = 1*2 + 2*4 + 3*1
    minimize
    [4,2,1] = 1*4 + 2*2 + 3*1

    two people
    [2,4,1,3] = 1*2         + 2*1
                    + 1*4           + 2*3
    minimize
    [4,3,2,1] = 1*4         + 2*2
                    + 1*3           + 2*1
 */
int main()
{
    int n, k = 0;
    string line = din.readLine().to!string;
    formattedRead(line, "%s %s", &n, &k);

    int[] costs = din.readLine().splitter().map!(to!int).array;
    int min_cost = 0;

    int factor = 0;
    int i = 0;
    foreach (flower; sort!"a > b"(costs)) {
        if (i % k == 0) {
            ++factor;
        }
        min_cost += factor * flower;
        ++i;
    }

    writeln(min_cost);

    return 0;
}