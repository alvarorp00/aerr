=% Array of booleans

int: N = 4;

array[1..N, 1..N] of var bool: S;

% Each column has exactly a cell with true value

constraint forall(i in 1..N) (sum([bool2int(S[j,i]) | j in 1..N]) == 1);

% Each diagonal has a similar number of true values

% Two diagonals --> right & left diagonals

constraint abs(sum([bool2int(S[i,i]) | i in 1..N])
                -
               sum([bool2int(S[N - i,N - i]) | i in 1..N])) == 0;

% Solve

solve satisfy;

output([
  show (if (S[i,j]) then 1 else 0 endif)++
    (if (j==N) then "\n" else " " endif) | i in 1..N,j in 1..N]);