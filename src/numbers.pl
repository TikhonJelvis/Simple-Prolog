% More examples from my slides.
number(zero).
number(s(X)) :- number(X).

% This is like a Haskell type; try it in GHCi:
% data ℕ = Z | S ℕ deriving (Show)

% Here's a really simple mathematical operation:
plus(zero, X, X).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).
