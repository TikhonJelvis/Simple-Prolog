% More examples from my slides.
number(zero).
number(s(X)) :- number(X).

% This is like a Haskell type; try it in GHCi:
% data ℕ = Z | S ℕ deriving (Show)

% Here's a really simple mathematical operation:
plus(zero, X, X).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).

% The way the recursion works is a bit odd—it feels a bit backwards—so
% make sure you understand how it works.

% Next, implement a few similar (but trickier!) operations
% yourself:
% - subtract (don't bother with negative numbers: 0 - x = 0)
% - multiply
% - compare (take two numbers and produce "ge", "le" or "eq")
%
% I've put the solutions in numberSolutions.pl.

