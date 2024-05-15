#!/usr/bin/env bash
#
# generates test data for the FraudulentActivityNotifications challenge
#
# ./fraud_gen.sh 1000 100 | ./FraudulentActivityNotifications
# ./fraud_gen.sh | ./FraudulentActivityNotifications

n=100000            # number of total expenses
d=100               # window size used for fraud detection
max_expense=200     # max amount for any expense

if [[ $1 ]]; then
    n="$1"
fi

if [[ $2 ]]; then
    d="$2"
fi

if [[ $3 ]]; then
    max_expense="$3"
fi

echo "$n $d"

for _ in $(seq 1 $n); do
    echo -n "$((1 + $SRANDOM % $max_expense)) "
done