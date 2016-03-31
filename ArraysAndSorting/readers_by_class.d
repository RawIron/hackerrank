import std.conv : to;
import std.cstream, std.stdio;
import std.algorithm;
import std.array;


class ArrayReader
{
  int[] read()
  {
    int numberOfEntries = to!(int)(din.readLine());
    int[] numbers = splitter(din.readLine().strip(' ')).map!(to!int).array;
    return numbers;
  }
}


class ArrayAndTestsReader : ArrayReader
{
  override int[] read()
  {
    int numberOfTests = to!(int)(din.readLine());
    return super.read();
  }
}


unittest
{
  auto reader = new ArrayReader();
  reader.read();
}

unittest
{
  auto reader = new ArrayAndTestsReader();
  reader.read();
}
