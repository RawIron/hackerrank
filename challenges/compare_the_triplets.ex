defmodule CompareTheTriplets do
@moduledoc """
practice Elixir recursion and the pipe operator
this code is inelegant
"""

  defmodule ReadWrite do
    @doc """
    into_list :: IO [String]
    """
    def into_list(), do: _read(IO.read(:stdio, :line), [])

    defp _read(:eof, lines), do: lines
    defp _read({:error, _reason}, _lines), do: []
    defp _read(data, lines), do: _read(IO.read(:stdio, :line), [data|lines])

    @doc """
    to_int_words :: [String] -> [[Int]]

    to_int_words(["2  7", "8   4"]) == [[2,7], [8,4]]
    """
    def to_int_words(lines) when is_list(lines) do
        lines
        |> _words([])
        |> _to_int
    end

    defp _words([], lists), do: lists
    defp _words([head|tail], lists), do: _words(tail, [String.split(head)|lists])

    defp _to_int([a,b|_tail]), do: [Enum.map(a, &String.to_integer(&1)), Enum.map(b, &String.to_integer(&1))]

    @doc """
    show({3,4}) -> 3 4
    """
    def show(result) when is_tuple(result) do
        :io.format("~B ~B", Tuple.to_list(result))
    end
  end


  defmodule Scores do
    @doc """
    count the wins of each of the two players
    by comparing their scores for every game in the list

    rate :: [(Int, Int)] -> (Int, Int)

    rate([{12,3}, {11,11}, {4,7}]) == {1,1}
    """
    def rate(games), do: _rate(games, 0, 0)

    defp _rate([], wins_a, wins_b), do: {wins_a, wins_b}
    defp _rate([{player_a, player_b} | tail], wins_a, wins_b) when player_a > player_b, do: _rate(tail, wins_a + 1, wins_b)
    defp _rate([{player_a, player_b} | tail], wins_a, wins_b) when player_b > player_a, do: _rate(tail, wins_a, wins_b + 1)
    defp _rate([{player_a, player_b} | tail], wins_a, wins_b) when player_b == player_a, do: _rate(tail, wins_a, wins_b)
  end
end


defmodule Solution do
    alias CompareTheTriplets.ReadWrite
    alias CompareTheTriplets.Scores

    ReadWrite.into_list
    |> ReadWrite.to_int_words
    |> List.zip
    |> Scores.rate
    |> ReadWrite.show
end
