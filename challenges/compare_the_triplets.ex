defmodule CompareTheTriplets do
  @doc """
  practice Elixir recursion and the pipe operator
  """

  defmodule ReadLines do
      def into_list(), do: _read(IO.read(:stdio, :line), [])

      defp _read(:eof, lines), do: lines
      defp _read({:error, _reason}, _lines), do: []   # TODO do not be silent
      defp _read(data, lines), do: _read(IO.read(:stdio, :line), [data|lines])
  end


  defmodule Sentences do
      def to_int_words(lines) do
          lines
          |> _words([])
          |> _to_int
      end

      defp _words([], lists), do: lists
      defp _words([head|tail], lists), do: _words(tail, [String.split(head)|lists])

      defp _to_int([a,b|[]]), do: [Enum.map(a, &String.to_integer(&1)), Enum.map(b, &String.to_integer(&1))]
  end


  defmodule Scores do
      def rate(players), do: _rate(players, 0, 0)

      defp _rate([], a, b), do: {a, b}
      defp _rate([{from_a, from_b} | tail], a, b) when from_a > from_b, do: _rate(tail, a+1, b)
      defp _rate([{from_a, from_b} | tail], a, b) when from_b > from_a, do: _rate(tail, a, b+1)
      defp _rate([{from_a, from_b} | tail], a, b) when from_b == from_a, do: _rate(tail, a, b)
  end


  defmodule Solution do
      result = ReadLines.into_list
          |> Sentences.to_int_words
          |> List.zip
          |> Scores.rate
      :io.format("~B ~B", Tuple.to_list(result))
  end

end
