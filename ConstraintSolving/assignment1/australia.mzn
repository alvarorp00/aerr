% N-Colour  --> Australia problem

int: nc = 3;  % colours will be numbered

% Regions: [wa, nt, sa, q, nsw, v, t]

% connections:

% WA: NT, SA
% NT: WA, SA, Q
% SA: WA, NT, Q, NSW, V
% Q: NT, SA, NSW
% NSW: SA, Q, V
% V: SA, NSW
% T: --

var 1..nc: wa;
var 1..nc: nt;
var 1..nc: sa;
var 1..nc: q;
var 1..nc: nsw;
var 1..nc: v;
var 1..nc: t;

constraint wa != nt;
constraint wa != sa;

constraint nt!= wa;
constraint nt!= sa;
constraint nt!= q;

constraint sa!= wa;
constraint sa!= nt;
constraint sa!= q;
constraint sa!= nsw;
constraint sa!= v;

constraint q != nt;
constraint q != sa;
constraint q != nsw;

constraint nsw != sa;
constraint nsw != q;
constraint nsw != v;

constraint v != sa;
constraint v != nsw;

solve satisfy;