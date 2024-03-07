defmodule HappyLadybugs do

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
  end


  defmodule DataByType do
      def next(:Int), do: IO.read(:stdio, :line) |> String.trim |> String.to_integer()
      def next(:String), do: IO.read(:stdio, :line) |> String.trim

      def print(false), do: IO.puts "NO"
      def print(true),  do: IO.puts "YES"
  end


  defmodule Counter do

      def create(keys) when is_list(keys) do
          _convert(keys, %{})
      end

      defp _convert([], counter), do: counter
      defp _convert([h|t], counter), do: _convert(t, update(counter, h))

      def update(counter, key) do
          case Map.has_key?(counter, key) do
              true  -> %{counter | key => counter[key]+1}
              false -> Map.put_new(counter, key, 1)
          end
      end
  end


  defmodule Apply do

    def while_false(enumerable, funcs) when is_list(funcs) and is_map(enumerable) do
      _while_false(funcs, enumerable, {:cont, {}})
    end

    defp _while_false([], _, {_, none}) do
      none
    end

    defp _while_false(_, _, {:halt, worked}) do
      worked
    end

    defp _while_false [head={_,func}|tail], enumerable, {:cont, none} do
      case func.(enumerable) do
        false  -> _while_false(tail, enumerable, {:cont, none})
        true -> _while_false(tail, enumerable, {:halt, head})
      end
    end

  end


  defmodule Streak do

    def length([]) do
      [0]
    end

    def length([head|tail]) do
      _length(tail, head, [1])
    end

    defp _length([], _, lengths) do
      lengths
    end

    defp _length([head|tail], on_left, [current|done]) do
      if head == on_left, do: _length(tail, head, [current+1|done]) , else: _length(tail, head, [1|[current|done]])
    end

  end


  defmodule Ladybug do
      # all empty
      #   => yes
      # none is empty
      #   one ladybug is not happy => no
      # one or more empty but not all
      #   no color shows exactly once => yes
      def all_empty?(counters) do
        case Map.keys counters do
          ["_"] -> true
          _ -> false
        end
      end

      def none_empty?(counters) do
        not Map.has_key?(counters, "_")
      end

      def is_happy(b) do
          require IEx
          IEx.pry
          game = String.split(b, "", trim: true)
          counters = game |> Counter.create()

          case Apply.while_false(counters, [{:all_empty, &(all_empty?(&1))},
                                            {:none_empty, &(none_empty?(&1))}])
          do
              {:all_empty, _} -> true
              {:none_empty, _} -> game |> Streak.length()
                                       |> Enum.all?(fn x -> x > 1 end)
              {} -> not (
                    counters
                    |> Enum.filter(fn {k,v} -> k != "_" end)
                    |> Enum.any?(fn {k,v} -> v == 1 end))
          end
      end
  end


  defmodule Solution do
      g = Data.next_int

      for n <- 1 .. g do
          n = Data.next_int
          b = Data.next_string

          Ladybug.is_happy(b)
          |> DataByType.print()
      end
  end

  end
