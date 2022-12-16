enum Tribes = {M, V};

% A restriction

var Tribes: A;
var Tribes: B;
var Tribes: C;

var bool:as;
var bool:bs;
var bool:cs;

constraint if A==M then not(as) else as endif;
constraint if B==M then not(bs) else bs endif;
constraint if C==M then not(cs) else cs endif;

constraint as == ((A==M /\ B==V /\ C==V) \/ (A==V /\ B==M /\ C==V) \/ (A==V /\ B==V /\ C==M));  % Just one is an M
constraint bs == ((A==M /\ B==M /\ C==V) \/ (A==V /\ B==M /\ C==M) \/ (A==M /\ B==V /\ C==M));  % Two are M
constraint cs == (A==M /\ B==M /\ C==M);  % All are M

% Try above again

% var bool:A;
% ...

% var bool:AS;
% constraint AS == (A+B+C == 1);
% constraint if A then not(DA) else DA endif; % model tribe with bool(int) value