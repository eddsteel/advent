:- initialization main, halt.
:- use_module(library(readutil)).

main :-
    read_input(Lines),
    star_one(Lines, Star1),
    write("answer 1: "),
    write(Star1), nl,
    star_two(Lines, Star2),
    write("answer 2: "),
    write(Star2), nl.

read_input(Lines) :-
    open('input/2', read, Input),
    read_file(Input, Lines),
    close(Input).

read_file(Stream, [H|L]) :-
    read_line_to_string(Stream, H),
    dif(H, end_of_file),
    read_file(Stream, L).

read_file(Stream, _) :- at_end_of_stream(Stream).

star_one(Lines, Star1) :-
    count_valid1(Lines, N), number_string(N, Star1).

star_two(Lines, Star2) :-
    count_valid2(Lines, N), number_string(N, Star2).

examples :- example1,example2,example3,
            star_one(["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"], "2"),
            star_two(["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"], "1").

example1 :-
    is_valid1(1, 3, 'a', "abcde"),
    is_valid_line1("1-3 a: abcde"),
    is_valid_line2("1-3 a: abcde").

example2 :-
    not(is_valid1(1, 3, 'b', "cdefg")),
    not(is_valid_line1("1-3 b: cdefg")),
    not(is_valid_line2("1-3 b: cdefg")).

example3 :-
    is_valid1(2, 9, 'c', "ccccccccc"),
    is_valid_line1("2-9 c: ccccccccc"),
    not(is_valid_line2("2-9 c: ccccccccc")).

count_valid1([], 0).
count_valid1([H|T], N) :- is_valid_line1(H), count_valid1(T, M), N is M + 1.
count_valid1([H|T], N) :- not(is_valid_line1(H)), count_valid1(T, N).

count_valid2([], 0).
count_valid2([H|T], N) :- is_valid_line2(H), count_valid2(T, M), N is M + 1.
count_valid2([H|T], N) :- not(is_valid_line2(H)), count_valid2(T, N).

explode(String, N1, N2, Atom, Candidate) :-
    split_string(String, ' ', ':', [Nums, Char, Candidate]),
    atom_string(Atom, Char),
    split_string(Nums, '-', '', [S1, S2]),
    number_string(N1, S1),
    number_string(N2, S2).

is_valid_line1(String) :-
    explode(String, Min, Max, Atom, Candidate),
    is_valid1(Min, Max, Atom, Candidate).

is_valid1(Min, Max, Atom, Candidate) :-
    count_char(Atom, Candidate, N),
    N >= Min,
    N =< Max.

count(_, [], 0).
count(Atom, [Atom|T], N) :- count(Atom, T, M), N is M + 1.
count(Atom, [H|T], N) :- dif(H, Atom), count(Atom, T, N).

count_char(Atom, String, N) :-
    atom(Atom),
    string(String),
    string_chars(String, Atoms),
    count(Atom, Atoms, N).

strpos(Atom, String, N) :- string_chars(String, Chars), pos(Atom, Chars, N).

pos(H, [H|_], 1).
pos(Char, [_|T], N) :- pos(Char, T, M), N is M + 1.

is_valid_line2(String) :-
    explode(String, Pos1, Pos2, Atom, Candidate),
    is_valid2(Pos1, Pos2, Atom, Candidate).

is_valid2(Pos1, Pos2, Atom, Candidate) :-
    strpos(Atom, Candidate, Pos1), not(strpos(Atom, Candidate, Pos2)).
is_valid2(Pos1, Pos2, Atom, Candidate) :-
    not(strpos(Atom, Candidate, Pos1)), strpos(Atom, Candidate, Pos2).

