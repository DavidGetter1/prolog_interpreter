=(X, X).

ehemann(christine, heinz).
ehemann(maria, fritz).
ehemann(monika, herbert).
ehemann(angelika, hubert).

mutter(herbert, christine).
mutter(angelika, christine).
mutter(hubert, maria).
mutter(susanne, monika).
mutter(norbert, monika).
mutter(andreas, angelika).

vater(K, V) :- ehemann(M, V), mutter(K, M).

elter(K, E) :- vater(K, E).
elter(K, E) :- mutter(K, E).

grossvater(E, G) :- elter(E, F), vater(F, G).

grossvaeter(Gs) :- findall([E, G], grossvater(E, G), Gs).

vorfahre(N, V) :- vorfahre(N, V2), vorfahre(V2, V).
vorfahre(N, V) :- elter(N, V).

geschwister(S, P) :- mutter(S, M), mutter(P,M), \+(=(P, S)).