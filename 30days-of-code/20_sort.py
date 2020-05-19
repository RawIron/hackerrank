#!/bin/python3


def bubble_sort_outofplace(in_numbers):
    # no in-place sort
    # copy the list first
    numbers = in_numbers[:]
    list_size = len(numbers)
    total_swaps = 0

    for i in range(list_size):
        swaps_counter = 0
        for j in range(1, list_size):
            if numbers[j-1] > numbers[j]:
                numbers[j], numbers[j-1] = numbers[j-1], numbers[j]
                swaps_counter += 1
        total_swaps += swaps_counter
        if swaps_counter == 0:
            break

    return numbers, total_swaps


def read_input():
    n = int(input().strip())
    return list(map(int, input().strip().split(' ')))


if __name__ == "__main__":
    # this is the global namespace !!
    in_numbers = read_input()

    bubble_sort = bubble_sort_outofplace
    sorted_numbers, num_swaps = bubble_sort(in_numbers)

    print(f"Array is sorted in {num_swaps} swaps.")
    print(f"First Element: {sorted_numbers[0]}")
    print(f"Last Element: {sorted_numbers[-1]}")
