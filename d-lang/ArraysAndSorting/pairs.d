#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


long pairs(in int[] that) pure
{
  long twoCombinations(in long r) {
    return r * (r-1);
  }

  if (that.length < 2) {
    return 0L;
  }

  long pairs = 0;
  int[int] counts;

  foreach (i; 0..that.length) {
    counts[that[i]] += 1;
  }

  foreach (key, value; counts) {
    if (value > 1) {
      pairs += twoCombinations(to!long(value));
    }
  }

  return pairs;
}


int main()
{
  //int numberOfEntries = 1;
  int numberOfTests = to!int(din.readLine()); 

  foreach (test; 0..numberOfTests) {
    int numberOfEntries = to!int(din.readLine());

    int[] population;
    population = splitter(din.readLine().strip(' ')).map!(to!int).array;
    //population = splitter("3 5 1 3 2 1 8 3".strip(' ')).map!(to!int).array;

    long solution = pairs(population);

    writeln(solution);
  }

  return 0;
}

