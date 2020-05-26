def calc_fees_nocal():
    '''
    ( 3, 1, 2021), (31, 12, 2019) == 10000
    (21, 4, 2021), (28,  3, 2020) == 10000
    ( 3, 1, 2020), (31, 12, 2019) == 500
    (21, 4, 2020), (28,  3, 2020) == 500
    (30, 3, 2020), (28,  3, 2020) == 30
    '''
    pass


def calculate_fees_unfair(received_at, due_at):
    '''
    (3, 1, 2020), ( 4, 12, 2019) == 10000
    (1, 3, 2020), (28,  2, 2020) == 500
    '''
    late_distances = [received - due for (received, due) in zip(received_at, due_at)]
    late_fees = [15, 500, 10000]

    total_fee = 0
    for (distance, fee) in zip(reversed(late_distances), reversed(late_fees)):
        if distance > 0:
            total_fee = min(distance * fee, 10000)
            break

    return total_fee


if __name__ == "__main__":
    # this is the global namespace !!
    received_date = map(int, input().split(' '))
    due_date = map(int, input().split(' '))

    calculate_fees = calculate_fees_unfair
    pay = calculate_fees(received_date, due_date)

    print(pay)
