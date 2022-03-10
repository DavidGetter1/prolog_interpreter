=(X,X).

factorial(0, 1).
factorial(N, F) :- >(N, 0), is(N1, -(N, 1)), factorial(N1, F1), is(F, *(F1, N)).