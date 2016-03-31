#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


void print(in int[int] that)
{
  foreach (key, value; that) {
    write(to!string(key), ":", to!string(value), " ");
  }
  writeln;
}


int[int] count(in int[] that)
{
  int[int] counts;

  foreach (entry; that) {
    counts[entry] += 1;
  }

  return counts;
}


int main()
{
  int numberOfEntries = 0;
  int[] population = new int[numberOfEntries];

  //numberOfEntries = to!int(din.readLine());
  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;

  population = splitter("3 5 3 2 8".strip(' ')).map!(to!int).array;

  int[int] counts = count(population);

  print(counts);

  return 0;
}

