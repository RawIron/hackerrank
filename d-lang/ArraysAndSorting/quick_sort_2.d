#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array, std.container;


void print(int[] sorted)
{
  foreach (entry; sorted) {
    write(to!string(entry) ~ " ");
  }
  writeln;
}


int[] swapIfReverse(int[] that, int left, int right)
{
  if (that[left] < that[right]) return that;
  else {
    int tmp = that[left];
    that[left] = that[right];
    that[right] = tmp;
    return that;
  }
}


int[] quicksort(int[] that)
{
  if (that.empty || that.length == 1) return that;
  else if (that.length == 2) swapIfReverse(that, 0, 1);
  
  int p = that[0];
  that = that[1..$];

  int[] left;
  int[] right;

  foreach (entry; that) {
    if (entry <= p)
      left ~= entry;
    else
      right ~= entry;
  }

  int[] leftSorted = quicksort(left);
  if (leftSorted.length > 1) print(leftSorted);
  int[] rightSorted = quicksort(right);
  if (rightSorted.length > 1) print(rightSorted);

  return leftSorted ~ p ~ rightSorted;
}


int main()
{
  int numberOfEntries = 0;
  int[] population = new int[numberOfEntries];

  //numberOfEntries = to!int(din.readLine());
  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;
  numberOfEntries = 7;
  population = splitter("5 8 1 3 7 9 2".strip(' ')).map!(to!int).array;

  int[] sorted = quicksort(population);

  print(sorted);

  return 0;
}

