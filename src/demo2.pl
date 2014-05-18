% More examples from my first few slides:
likes(alice, pear).
likes(alice, orange).
female(alice).
likes(bob, pear).
likes(bob, banana).
male(bob).

% Added a few more facts:
likes(charles, X) :- fruit(X).
likes(dan, X) :- fruit(X), yellow(X).

likes(eve, X) :- fruit(X); vegetable(X).

fruit(pear).
fruit(orange).
fruit(banana).

yellow(banana).

% Enough with fruits! Now to talk about numbers in numbers.pl.