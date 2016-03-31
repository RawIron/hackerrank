#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


void print(int[] sorted)
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


int main() {
  int numberOfEntries = 0;
  int[] population = new int[numberOfEntries];
  int shifts = 0;

  //numberOfEntries = to!int(din.readLine());
  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;
  numberOfEntries = 4;
  population = splitter("3 5 2 8".strip(' ')).map!(to!int).array;

  shifts = insertionSort(population);

  print(population);
  writeln(shifts);

  return 0;
}

