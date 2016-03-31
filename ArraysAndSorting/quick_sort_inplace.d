#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


int[] emptyArray;


enum ptree = false;

struct Print {
  enum tabSize = 2;
  int height = 0;
  char[tabSize] tab = "  ";
  char[] ident;

  void dive() {
    ++height;
    ident ~= tab;
  }
}

void print(int[] sorted)
{
  foreach (entry; sorted) {
    write(to!string(entry) ~ " ");
  }
  writeln;
}


void swap(int[] that, int i, int j)
{
  int tmp = that[i];
  that[i] = that[j];
  that[j] = tmp;
}


int[] quicksort(int[] that)
{
  int sortPartition(int[] that, int left, int p, int right)
  {
    if (left > right) return -1;

    int cutHere = 0;
    int i = left;
    while (i < left + that[left..right+1].length) {
      if (that[i] <= that[p]) {
        swap(that, i, left + cutHere);
        ++cutHere;
      }
      else {
        bool foundSwap = false;
        int j = i+1;
        while (!foundSwap && j<=right) {
          if (that[j] <= that[p]) {
            swap(that, i, j);
            foundSwap = true;
            ++cutHere;
          }
          ++j;
        }
      }
      ++i;
    }

    if (cutHere < that[left..right+1].length) {
      swap(that, left + cutHere, right+1);
    }

    return cutHere;
  }

  int[] inplacePrint(int[] that, int left, int p, int right, Print d)
  {
    writefln("%s called %d %d %d", d.ident, left, p, right);

    int cutHere = sortPartition(that, left, p, right);
    d.dive();
    writef("%s root ", d.ident);
    writefln("%s", that);

    if (cutHere >= 0) {
      inplacePrint(that, left, left + cutHere-1, left + cutHere-2, d);
      writef("%s left ", d.ident);
      writefln("%s", that);

      inplacePrint(that, left + cutHere+1, right+1, right, d);
      writef("%s right ", d.ident);
      writefln("%s", that);
    }

    return that;
  }

  int[] inplace(int[] that, int left, int p, int right)
  {
    int cutHere = sortPartition(that, left, p, right);

    if (cutHere >= 0) {
      inplace(that, left, left + cutHere-1, left + cutHere-2);
      inplace(that, left + cutHere+1, right+1, right);
    }

    return that;
  }

  static if (ptree) {
    Print d;
    if (that.length <= 1) return that;
    else return inplacePrint(that, 0, to!int(that.length-1), to!int(that.length-2), d);
  }
  else {
    if (that.length <= 1) return that;
    else return inplace(that, 0, to!int(that.length-1), to!int(that.length-2));
  }
}


unittest
{
  int[] population = [1];
  int[] expected = [1];
  assert(quicksort(population) == expected);
}

unittest
{
  int[] population = [0,0];
  int[] expected = [0,0];
  assert(quicksort(population) == expected);
}

unittest
{
  int[] population = [0,1];
  int[] expected = [0,1];
  assert(quicksort(population) == expected);
}

unittest
{
  int[] population = [1,0];
  int[] expected = [0,1];
  assert(quicksort(population) == expected);
}

unittest
{
  int[] population = [0,1,2];
  int[] expected = [0,1,2];
  assert(quicksort(population) == expected);
}

unittest
{
  int[] population = [0,2,1];
  int[] expected = [0,1,2];
  assert(quicksort(population) == expected);
}

unittest
{
  int[] population = [1,2,0];
  int[] expected = [0,1,2];
  assert(quicksort(population) == expected);
}

unittest
{
  int[] population = [1,0,2];
  int[] expected = [0,1,2];
  assert(quicksort(population) == expected);
}

unittest
{
  int[] population = [2,1,0];
  int[] expected = [0,1,2];
  assert(quicksort(population) == expected);
}


int main()
{
  int numberOfEntries = 0;
  int[] population = new int[numberOfEntries];

  //numberOfEntries = to!int(din.readLine());
  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;

  population = splitter("5 8 1 3 7 9 2".strip(' ')).map!(to!int).array;
  //population = splitter("5 8 9 7 10 2".strip(' ')).map!(to!int).array;

  writeln(population);
  quicksort(population);
  writeln(population);

  return 0;
}

