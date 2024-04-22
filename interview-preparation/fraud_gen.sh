#!/usr/bin/env bash
#
# generates test data for the FraudulentActivityNotifications challenge
#
# ./fraud_gen.sh 100 100 | ./FraudulentActivityNotifications
# ./fraud_gen.sh | ./FraudulentActivityNotifications

n=100000
d=100
expense=200

if [[ $1 ]]; then
    n="$1"
fi

if [[ $2 ]]; then
    d="$2"
fi

echo "$n $d"

for _ in $(seq 1 $n); do
    echo -n "$((1 + $SRANDOM % $expense)) "
done