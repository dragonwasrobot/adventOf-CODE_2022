#!/usr/local/bin/swipl -q

:- use_module(library(readutil)).
:- use_module(library(apply)).

:- initialization main.

% take until from list until encountering C
take_until([], _, [], []).
take_until([X|Xs], X, [], Xs).

take_until([X|Xs], C, [X|Ps], Rs) :-
    X \= C,
    take_until(Xs, C, Ps, Rs).

% Splits a list ["1", "2", C, "3", "", "4"]
% into [["1", "2"], ["3", "4"]]
% where C is any separator string, e.g. ""
split(_, [], []) .

split(C, [X|Xs], [Y|Ys]) :-
    take_until([X|Xs], C, Y, Rs),
    split(C, Rs, Ys).

% parses a list ["1", "2", "3"]
% into its integer sum 6.
parse_and_sum([], 0).
parse_and_sum([X|Xs], R) :-
    parse_and_sum(Xs, Ys),
    number_string(N, X),
    R is N + Ys.

parse_list([], []).
parse_list([X | Xs], [XSum | Rs]) :-
    parse_and_sum(X, XSum),
    parse_list(Xs, Rs).

% takes the first N items from the input list
acc_take(_, [], Xs, Xs).
acc_take(0, _, Xs, Xs).

acc_take(N, [X|Xs], Ys, [X|Rs]) :-
    M is N - 1,
    acc_take(M, Xs, Ys, Rs).

take(N, Xs, Rs) :-
    acc_take(N, Xs, [], Rs).

% Solves the puzzles and prints the result
main :-
    read_file_to_string("day1-input.txt", RawInput, []),
    split_string(RawInput, "\n", "", StringList),
    split("", StringList, TupleList),
    parse_list(TupleList, NumberList),
    sort(0, @>=, NumberList, SortedList),
    max_list(SortedList, Solution1),
    take(3, SortedList, FirstThree),
    sum_list(FirstThree, Solution2),
    format('Solution 1: ~w\n', [Solution1]), % 68467
    format('Solution 2: ~w\n', [Solution2]), % 203420
    halt(0).
