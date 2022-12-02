#!/usr/bin/env bash

set -ef -o pipefail

total1=0
total2=0
left=""
right=""

# Not particularly elegant nested case approach but it gets the job done.
function compute_scores() {
    local score1=0
    local score2=0

    case $left in
        A)
            case $right in
                X)
                    score1=$((3 + 1))
                    score2=$((0 + 3))
                    ;;
                Y)
                    score1=$((6 + 2))
                    score2=$((3 + 1))
                    ;;
                Z)
                    score1=$((0 + 3))
                    score2=$((6 + 2))
                    ;;
            esac
            ;;

        B)
            case $right in
                X)
                    score1=$((0 + 1))
                    score2=$((0 + 1))
                    ;;
                Y)
                    score1=$((3 + 2))
                    score2=$((3 + 2))
                    ;;
                Z)
                    score1=$((6 + 3))
                    score2=$((6 + 3))
                    ;;
            esac
            ;;

        C)
            case $right in
                X)
                    score1=$((6 + 1))
                    score2=$((0 + 2))
                    ;;
                Y)
                    score1=$((0 + 2))
                    score2=$((3 + 3))
                    ;;
                Z)
                    score1=$((3 + 3))
                    score2=$((6 + 1))
                    ;;
            esac
            ;;
    esac

    total1=$(($total1 + $score1))
    total2=$(($total2 + $score2))
}

# Read input line by line and calculate the
# two different scores to their respective totals.
while read line; do
    parts=($(echo $line | tr " " "\n"))
    left="${parts[0]}"
    right="${parts[1]}"
    compute_scores
done <day2-input.txt

echo "Total 1: ${total1}" # 12679
echo "Total 2: ${total2}" # 14470
