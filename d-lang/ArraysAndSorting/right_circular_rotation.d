#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


int[] rotate(int[] that, in int r)
{
  if (r == 0) return that;
  else if (that.length < 2) return that;
  else if (that.length == r) return that;

  int rotate = to!int(r % that.length);
  return that[$-rotate..$] ~ that[0..$-rotate];
}

unittest
{
  int[] numbers = [3, 2];
  assert(rotate(numbers, 1) == [2, 3]);
}

unittest
{
  int[] numbers = [3, 2];
  assert(rotate(numbers, 2) == [3, 2]);
}


int main()
{
  int numberOfEntries = 1;
  //numberOfEntries = to!int(din.readLine());
  int[] population;

  int numberOfRotations = 1;
  //numberOfRotations = to!int(din.readLine());

  //population = splitter(din.readLine().strip(' ')).map!(to!int).array;
  population = splitter("3 5 3 2 8".strip(' ')).map!(to!int).array;

  int[] solution = rotate(population, numberOfRotations);

  int position = 1;
  //position = to!int(din.readLine());

  writeln(solution[position]);

  return 0;
}

