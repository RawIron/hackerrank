def solve(lengths):
    """
    a bar has several bumper plates of different weights/diameter
        -4-6-8-3-1-
    in the above example the plate "4" or the plate "1" can be removed from the bar
    can smaller bumper plates always be stacked on larger bumper plates
    until there are no plates left on the bar?

    on every move take the larger of the two accessable bumper plates from the bar
    and put it on top of the previous plate
    """
    if not isinstance(lengths, list):
        lengths = list(lengths)

    plate_on_top = 2**31    # weight of the plate on top of the stack
    l = 0                   # accessable plate on the left
    r = len(lengths)-1      # accessable plate on the right

    while l <= r:
        take_left_plate = (lengths[l] >= lengths[r])
        if take_left_plate:
            picked = lengths[l]
            l += 1
        else:
            picked = lengths[r]
            r -= 1

        if picked > plate_on_top:
            break

        plate_on_top = picked

    bar_has_plates = (l <= r)
    if bar_has_plates:
        return False
    else:
        return True

def read():
    n = int(input())
    return map(int, input().split())

def main():
    t = int(input())
    for _ in range(0,t):
        lengths = read()

        can_be_solved = solve(lengths)
        if can_be_solved:
            print("Yes")
        else:
            print("No")

main()
