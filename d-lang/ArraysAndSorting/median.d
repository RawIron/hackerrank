#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.algorithm;
import std.array;

alias index = ulong;


int median(in int[] that)
{
  if (that.length == 1) return that[0];
  
  int part(in int[] partition, in index indexOfMedian)
  {
    int p = partition[0];
    int[] left;
    int[] right;

    foreach (entry; partition[1..$]) {
      if (entry < p)
        left ~= entry;
      else
        right ~= entry;
    }
    left ~= p;

    // all remaining entries have value p
    if (left.length + right.length == 1) return p;

    // pivot is at median
    if (left.length-1 == indexOfMedian) return p;

    if (left.length-1 < indexOfMedian)
      return part(right, indexOfMedian - left.length);

    else
      return part(left, indexOfMedian);
  }

  return part(that, that.length / 2);
}


unittest
{
  int[] numbers = [1, 2, 5, 6, 4];
  assert(median(numbers) == 4);
}

unittest
{
  int[] numbers = [2, 10, 7, 1, 8, 9, 3];
  assert(median(numbers) == 7);
}

unittest
{
  int[] numbers = [2, 10, 7];
  assert(median(numbers) == 7);
}

unittest
{
  int[] numbers = [7, 10, 2];
  assert(median(numbers) == 7);
}


int main()
{
  int numberOfEntries = 0;
  //numberOfEntries = to!int(din.readLine());
  //int[] population = new int[numberOfEntries];

  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;
  //population = splitter("5 8 1 3 7 9 2".strip(' ')).map!(to!int).array;
  int[] population = [1, 2, 5, 6, 4];

  int solution = median(population);

  writeln(solution);

  return 0;
}

