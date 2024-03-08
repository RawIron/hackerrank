@moduledoc """
The implementiation in this file is very inelegant.
It was more about learn a bit of Elixir.
"""

defmodule Data do
  def next_ints() do
      IO.read(:stdio, :line) |> String.split() |> Enum.map(&String.to_integer(&1))
  end

  def next_int() do
      IO.read(:stdio, :line) |> String.trim |> String.to_integer()
  end

  def next_string() do
      IO.read(:stdio, :line) |> String.trim
  end

  def next(:Int), do: IO.read(:stdio, :line) |> String.trim |> String.to_integer()
  def next(:String), do: IO.read(:stdio, :line) |> String.trim

  def print(false), do: IO.puts "NO"
  def print(true),  do: IO.puts "YES"
end


defmodule Counter do
  @doc """
  create a frequency map from a list

  create :: [a] -> Map a int

  ["the", "car", "the"] == %{"the": 2, "car": 1}
  "baebe" == %{'a': 1, 'b': 2, 'e': 2}
  """
  def create(keys) when is_list(keys) do
      _convert(keys, %{})
  end

  defp _convert([], frequencies), do: frequencies
  defp _convert([h|t], frequencies), do: _convert(t, update(frequencies, h))

  @doc """
  increase the frequency by 1 for the given key
  a missing key is added with a frequency of 1

  update :: Map a int -> a -> Map a int
  """
  def update(frequencies, key) do
      case Map.has_key?(frequencies, key) do
          true  -> %{frequencies | key => frequencies[key]+1}
          false -> Map.put_new(frequencies, key, 1)
      end
  end
end


defmodule Streak do
  @doc """
  calculate the length of every streak within a list

  length([2,2,2,1,3,5,5,5]) == [3,1,1,3]
  length("aaaaceee") == [4,1,3]
  """
  def length([]) do
    [0]
  end

  def length([head|tail]) do
    _length(tail, head, [1])
  end

  defp _length([], _, lengths) do
    lengths
  end

  defp _length([head|tail], streak_value, [current|done]) do
    if head == streak_value do
      _length(tail, streak_value, [current+1|done])
    else
      _length(tail, head, [1|[current|done]])
    end
  end
end


defmodule Apply do
  @doc """
  find the first predicate which evaluates to true for a map

  while_false :: Map String a -> [(:function_id, Map String a -> bool)] -> bool
  """
  def while_false(a_map, funcs) when is_list(funcs) and is_map(a_map) do
    _while_false(a_map, funcs, {:cont, :none})
  end

  defp _while_false(_, [], {_, none}) do
    none
  end
  defp _while_false(_, _, {:halt, fun_id}) do
    fun_id
  end
  defp _while_false(a_map, [{fun_id,func}|tail], {:cont, none}) do
    case func.(a_map) do
      false -> _while_false(a_map, tail, {:cont, none})
      true  -> _while_false(a_map, tail, {:halt, fun_id})
    end
  end
end


defmodule Ladybug do
  @moduledoc """
  a simple board game called Ladybug
  the goal is to make all the ladybugs on the board happy

  a ladybug with another ladybug of its kind at its side is happy
  the board is a single row of squares for the ladybugs to land on

  ladybugs of the same kind are marked with the same character
  an empty plot has an "_" on it

  example:
  the board has 7 plots and only the B-ladybugs are happy
  BB_CD_C
  """

  @doc """
  true :  there is only one key in the map
          and it is the "_"
  false : otherwise
  """
  def all_empty?(counters) do
    case Map.keys counters do
      ["_"] -> true
      _ -> false
    end
  end

  @doc """
  true :  "_" is not in the keys of the map
  false : otherwise
  """
  def none_empty?(counters) do
    not Map.has_key?(counters, "_")
  end

  @doc """
  is there a sequence of moves for the ladybugs
  on the board such that all ladybugs are happy
  afterwards

  is_happy(X_Y__X) == NO
  is_happy(AABCBC) == NO
  is_happy(__) == YES
  is_happy(DD__FQ_QQF) == YES
  is_happy(B_RRBR) == YES
  """
  def is_happy(game) do
      counters = game |> Counter.create()
      predicates = [{:all_empty, &(all_empty?(&1))},
                    {:none_empty, &(none_empty?(&1))}
                  ]
      case Apply.while_false(counters, predicates) do
        #
        :all_empty  -> true
        # one ladybug is not happy => no
        :none_empty -> game |> Streak.length()
                            |> Enum.all?(fn x -> x > 1 end)
        # one or more empty but not all
        #   no color shows exactly once => yes
        :none -> not (
              counters
              |> Enum.filter(fn {k,_v} -> k != "_" end)
              |> Enum.any?(fn {_k,v} -> v == 1 end)
              )
      end
  end
end


defmodule Main do
  g = Data.next_int

  for _ <- 1 .. g do
    _ = Data.next_int

    Data.next_string
    |> String.split("", trim: true)
    |> Ladybug.is_happy()
    |> Data.print()
  end
end
