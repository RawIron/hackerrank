defmodule AppleAndOrange do
    @moduledoc """
    Fruits fall from a tree
    and land some distance away from the tree.

    There are two trees and one house in a garden.
    The trees are on location _a_ and _o_
    and the house stretches from _hl to _hh.

    1 -------- + -------- + -------- 30
         ^       <======>      ^
         a      ha     ho      o

    1<= a < ha < ho < o
    """

    defmodule Data do
        def next_line() do
            IO.read(:stdio, :line)
            |> String.split()
            |> Enum.map(&String.to_integer(&1))
        end

        def show(value) when is_integer(value) do
            :io.fwrite("~B~n", [value])
        end
    end

    @doc """
    how many of the fruits fell on the house?

    count_hits([[2,3,-4], 4], [7,10]) == 1
    """
    def count_hits([distances, tree], [ha, ho]) do
        [left, right] = [ha-tree, ho-tree]
        is_hit = fn(d) -> left <= d and d <= right end

        distances
        |> Enum.filter(is_hit)
        |> Enum.count()
    end

end


defmodule Solution do
    alias AppleAndOrange.Data

    house = Data.next_line()
    [apple_tree, orange_tree] = Data.next_line()
    [_m, _n] = Data.next_line()
    apples = Data.next_line()
    oranges = Data.next_line()

    [[apples, apple_tree], [oranges, orange_tree]]
    |> Enum.map(&AppleAndOrange.count_hits(&1, house))
    |> Enum.map(&Data.show(&1))
end
