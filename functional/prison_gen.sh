#!/usr/bin/env bash
#
# generates test data for the PrisonTransport challenge
#
# ./prison_gen.sh 100 100 | ./PrisonTransport
# ./prison_gen.sh | ./PrisonTransport

n=100000
m=100000

if [[ $1 ]]; then
    n="$1"
fi

if [[ $2 ]]; then
    m="$2"
fi

echo $n
echo $m

for _ in $(seq 1 $m); do
    echo $((1 + $SRANDOM % $n)) $((1 + $SRANDOM % $n))
done
