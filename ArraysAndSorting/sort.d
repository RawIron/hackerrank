#!/usr/bin/rdmd

import std.conv : to;
import std.stdio, std.cstream, std.algorithm;
import std.array;


enum Env: int {local, submit};
enum What: int {totalEntries, entries};
const Env myEnv = Env.local;

enum Sort: int {insertion, quick};
const Sort mySort = Sort.insertion;


template match(Env e : Env.submit)
{
  string data(What w) {
    return to!string(din.readLine());
  }

  void print(int[] sorted) {
  }
}

template match(Env e : Env.local)
{
  string data(What w) {
    final switch (w) {
    case What.entries: return "5 3 1";
    case What.totalEntries: return "3";
    }
  }

  void print(int[] sorted) {
    foreach (entry; sorted) {
      writeln(entry);
    }
  }
}


string reader(What w)
{
  return match!(myEnv).data(w);
}

void print(int[] sorted)
{
  match!(myEnv).print(sorted);
}


template sort(Sort s : Sort.insertion)
{
  int order(int[] that)
  {
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
}


int main()
{
  int numberOfEntries = 0;
  int[] population = new int[numberOfEntries];

  numberOfEntries = to!int(reader(What.totalEntries));
  population = splitter(reader(What.entries).strip(' ')).map!(to!int).array;

  sort!(mySort).order(population);

  print(population);

  return 0;
}

