less(i,i(_)).
less(i,o(_)).
less(o(N), i(N)).
less(o(N), o(M)) :- less(N,M).
less(i(N), i(M)) :- less(N,M).
less(i(N), o(M)) :- less(N,M).
less(o(N), i(M)) :- less(N,M).

lessTest(A,B,C) :- less(A,C), less(B,C).