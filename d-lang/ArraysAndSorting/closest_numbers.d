#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


void print(in interval[] that)
{
  foreach (value; that) {
    write(to!string(value.a), " ", to!string(value.b), " ");
  }
  writeln;
}


struct interval { int a; int b; };

interval[] closest(int[] that)
{
  interval pair;
  interval[] pairs;
  int minDistance = int.max;

  auto sorted = sort(that);

  foreach (i; 1..sorted.length) {
    int distance = sorted[i] - sorted[i-1]; 
    if (distance <= minDistance) {
      if (distance < minDistance) pairs.length = 0;
      pair.a = sorted[i-1];
      pair.b = sorted[i];
      pairs ~= pair;
      minDistance = distance;
    }
  }

  return pairs;
}


int main()
{
  int numberOfEntries = 1;
  //numberOfEntries = to!int(din.readLine());

  int[] population = new int[numberOfEntries];

  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;
  population = splitter("3 5 3 2 8".strip(' ')).map!(to!int).array;

  interval[] solution = closest(population);

  print(solution);

  return 0;
}

