#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


void print(in int[] sorted)
{
  foreach (entry; sorted) {
    write(to!string(entry) ~ " ");
  }
  writeln;
}


int insertionSort(int[] that) {
  int keep, j;
  int shifts = 0;

  for (int i=1; i < that.length; i++) {
    keep = that[i];
    for (j = i-1; j >= 0 && that[j] > keep; j--) {
      that[j+1] = that[j];
      shifts++;
    }
    that[j+1] = keep;
  }

  return shifts;
}


int quicksort(int[] that)
{
  void swap(const int i, const int j)
  {
    int tmp = that[i];
    that[i] = that[j];
    that[j] = tmp;
  }

  int sortPartition(const int left, const int p, const int right)
  {
    if (left > right) return -1;

    int cutHere = 0;
    int i = left;
    while (i < left + that[left..right+1].length) {
      if (that[i] <= that[p]) {
        swap(i, left + cutHere);
        ++cutHere;
      }
      else {
        bool foundSwap = false;
        int j = i+1;
        while (!foundSwap && j<=right) {
          if (that[j] <= that[p]) {
            swap(i, j);
            foundSwap = true;
            ++cutHere;
          }
          ++j;
        }
      }
      ++i;
    }

    swap(left + cutHere, right+1);

    return cutHere;
  }

  int swaps = 0;

  int inplace(const int left, const int p, const int right)
  {
    int cutHere = sortPartition(left, p, right);

    if (cutHere >= 0) {
      swaps += cutHere+1;

      inplace(left, left + cutHere-1, left + cutHere-2);
      inplace(left + cutHere+1, right+1, right);
    }

    return swaps;
  }

  if (that.length <= 1) return 0;
  else return inplace(0, to!int(that.length-1), to!int(that.length-2));
}


int main()
{
  int numberOfEntries = 0;
  int[] population = new int[numberOfEntries];

  //numberOfEntries = to!int(din.readLine());
  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;

  //population = splitter("1 3 9 8 2 7 5".strip(' ')).map!(to!int).array;
  population = splitter("10 9 8 7 6 5 4 3 2 1".strip(' ')).map!(to!int).array;
  //population = splitter("5 8 1 3 7 9 2".strip(' ')).map!(to!int).array;
  //population = splitter("5 8 9 7 10 2".strip(' ')).map!(to!int).array;

  int swaps = quicksort(population.dup);
  int shifts = insertionSort(population);

  writeln(shifts - swaps);

  return 0;
}

