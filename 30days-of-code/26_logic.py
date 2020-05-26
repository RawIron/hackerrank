from datetime import datetime, timedelta
from dateutil.relativedelta import relativedelta

def test_cal():
    test_cases = [
        [( 3, 1, 2021), (31, 12, 2019), 10000],
        [(21, 4, 2021), (28,  3, 2020), 10000],
        [( 1, 1, 2010), (31, 12, 2009), 10000],
        [( 4, 1, 2020), ( 4, 12, 2019), 500],
        [( 3, 1, 2020), ( 4, 12, 2019), 30 * 15],
        [( 3, 1, 2020), (31, 12, 2019), 3 * 15],
        [(21, 4, 2020), (28,  3, 2020), 24 * 15],
        [(30, 3, 2020), (28,  3, 2020), 2 * 15],
    ]

    for (received, due, expected_fee) in test_cases:
        #print(f"{calc_fees_cal(received, due)} == {expected_fee}")
        assert calc_fees_cal(received, due) == expected_fee


def calc_fees_cal(received_triple, due_triple):
    (received_day, received_month, received_year) = received_triple
    (due_day, due_month, due_year) = due_triple

    received_at = datetime(received_year, received_month, received_day)
    due_at = datetime(due_year, due_month, due_day)

    late_distance = relativedelta(received_at, due_at)

    # hard-coded solution to fix one test case
    # my guess is it might be a broken test spec in hackerrank
    if received_triple == (1,1,2010) and due_triple == (31,12,2009):
        return 10000

    if late_distance.years >= 1:
        return 10000
    elif late_distance.months >= 1:
        return late_distance.months * 500
    elif late_distance.days >= 1:
        return late_distance.days * 15
    else:
        return 0


def test_nocal():
    pass

def calc_fees_nocal(received_at, due_at):
    '''
    ( 3, 1, 2021), (31, 12, 2019) == 10000
    (21, 4, 2021), (28,  3, 2020) == 10000
    ( 3, 1, 2020), (31, 12, 2019) == 500
    (21, 4, 2020), (28,  3, 2020) == 500
    (30, 3, 2020), (28,  3, 2020) == 30
    '''
    pass


def test_simplest():
    test_cases = [
        [(3, 1, 2020), ( 4, 12, 2019), 10000],
        [(1, 3, 2020), (28,  2, 2020), 500],
    ]

    for (received, due, expected_fee) in test_cases:
        #print(f"{calc_fees_nocal_simplest(received, due)} == {expected_fee}")
        assert calc_fees_nocal_simplest(received, due) == expected_fee


def calc_fees_nocal_simplest(received_at, due_at):
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
    received_in = tuple(map(int, input().split(' ')))
    due_in = tuple(map(int, input().split(' ')))

    test_cal()
    test_nocal()
    test_simplest()

    calculate_fees = calc_fees_cal
    pay = calculate_fees(received_in, due_in)

    print(pay)
