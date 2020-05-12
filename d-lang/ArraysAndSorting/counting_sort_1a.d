#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


void print(in int[] that)
{
  foreach (value; that) {
    write(to!string(value), " ");
  }
  writeln;
}

void printSorted(in int[] that)
{
  int i = 0;
  while (i < that.length) {
    int j = 0;
    while (j < that[i]) {
      write(to!string(i), " ");
      ++j;
    }
    ++i;
  }
  writeln;
}

void printTotal(in int[] that)
{
  int total = 0;
  foreach (value; that) {
    total += value;
    write(to!string(total), " ");
  }
  writeln;
}


int[] count(in int[] that)
{
  int[100] counts = 0;

  foreach (entry; that) {
    counts[entry] += 1;
  }

  return counts.array;
}


int main()
{
  int numberOfEntries = 1;
  int[] population = new int[numberOfEntries];

  //numberOfEntries = to!int(din.readLine());
  foreach (i; 0..numberOfEntries) {
  population[i] = splitter("3 5".strip(' ')).array[0].to!int;
    //population[i] = splitter(din.readLine().strip(' ')).array[0].to!int;
  }

  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;
  //population = splitter("3 5 3 2 8".strip(' ')).map!(to!int).array;

  int[] counts = count(population);

  printTotal(counts);

  return 0;
}

