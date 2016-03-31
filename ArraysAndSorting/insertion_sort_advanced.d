#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


int calcShifts(int[] that) {
  int shifts = 0;
  int total = 0;

  foreach (i; 1..that.length) {
    if (total + that[i] - that[i-1] < 0) {
      shifts += i;
    }
    total += that[i] - that[i-1];
  }

  return shifts;
}


unittest
{
  int[] that = [3, 5, 2, 8];
  assert(calcShifts(that) == 2);
}

unittest
{
  int[] that = [8, 4, 10, 2, 1, 6, 7, 12];
  assert(calcShifts(that) == 12);
}


int main() {
  int numberOfTests = 1;
  int shifts = 0;

  foreach (test; 0..numberOfTests) {
    //int numberOfEntries = to!int(din.readLine());
    int[] population = new int[numberOfEntries];
    //population = splitter(din.readLine().strip(' ')).map!(to!int).array;
    //population = splitter("3 5 2 8".strip(' ')).map!(to!int).array;
    population = splitter("8 4 10 2 1 6 7 12".strip(' ')).map!(to!int).array;

    shifts = calcShifts(population);

    writeln(shifts);
  }

  return 0;
}

