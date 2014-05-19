% Note how this is actually just plus with the arguments in a
% different order! This is because a + b = c also means that
% c - a = b.
subtract(X, zero, X).
subtract(s(N), s(X), Z) :- subtract(N, X, Z).

plus(zero, X, X).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).

% This one is actually very similar to plus.
multiply(X, zero, zero).
multiply(X, s(Y), Z) :- multiply(X, Y, T), plus(X, T, Z).

% Just like plus and subtract, you can use multiply to divide (but
% only things which are perfectly divisible). Try dividing four by two:
%
% ?- multiply(s(s(zero)), X, s(s(s(s(zero))))).
