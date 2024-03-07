defmodule MiniMax do
  def sum_take4(numbers) do
      import Enum, only: [take: 2, sum: 1]

      is_sorted = Enum.sort numbers

      {is_sorted |> take(4) |> sum,
       is_sorted |> take(-4) |> sum}
  end

  def read_input() do
    IO.read(:stdio, :line)
    |> String.split()
    |> Enum.map(&String.to_integer(&1))
  end

  def show(mini_max) do
    :io.format("~B ~B", Tuple.to_list(mini_max))
  end
end


defmodule Main do
  import MiniMax
  read_input()
  |> sum_take4()
  |> show()
end
