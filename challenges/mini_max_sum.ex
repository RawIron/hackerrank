defmodule MiniMaxSum do

  defmodule MiniMax do
      def sum_take4(numbers) do
          import Enum, only: [take: 2, sum: 1]

          is_sorted = Enum.sort numbers

          {is_sorted |> take(4) |> sum,
           is_sorted |> take(-4) |> sum}
      end
  end

  defmodule Solution do
      result = IO.read(:stdio, :line)
      |> String.split()
      |> Enum.map(&String.to_integer(&1))
      |> MiniMax.sum_take4()

      :io.format("~B ~B", Tuple.to_list(result))
  end

end
