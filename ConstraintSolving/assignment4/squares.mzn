% A square of 1 to N, place the numbers 1, 2 .. N² in such a way that every row and every column sum the same amount.

int: N = 3;

array[1..N, 1..N] of var 1..N*N: X;
var int: total;

include "alldifferent.mzn";

constraint alldifferent(X);
constraint forall(i in 1..N)(sum([(X[i, j]) | j in 1..N]) = total);
constraint forall(j in 1..N)(sum([(X[i, j]) | i in 1..N]) = total);

solve satisfy;