defmodule AppleAndOrange do

  defmodule Data do
      def next_line() do
          IO.read(:stdio, :line) |> String.split() |> Enum.map(&String.to_integer(&1))
      end
  end

  defmodule AppleOrange do

      def count_hits([apples, oranges], [s, t], [a, b]) do
          [_count(apples, [s,t], a), _count(oranges, [s,t], b)]
      end

      defp _count(fruits, [s, t], tree) do
          [left, right] = [s-tree, t-tree]
          is_hit = fn(x) -> left <= x and x <= right end

          fruits
          |> Enum.filter(is_hit)
          |> Enum.count()
      end

  end

  defmodule Solution do

      house = Data.next_line()
      trees = Data.next_line()
      [m, n] = Data.next_line()
      apples = Data.next_line()
      oranges = Data.next_line()

      [apple_count, orange_count] = AppleOrange.count_hits([apples, oranges], house, trees)

      :io.format("~B~n", [apple_count])
      :io.format("~B", [orange_count])
  end

end
