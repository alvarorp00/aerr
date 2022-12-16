%   send
% + more
% ------
%  money

include "alldifferent.mzn";

var 1..9: e;
var 0..9: d;
var 1..9: m;
var 0..9: n;
var 0..9: r;
var 0..9: o;
var 0..9: s;
var 0..9: y;

constraint alldifferent([e,d,m,n,o,s,y]);

constraint s * 1000 + e * 100 + n * 10 + d + m * 1000 + o * 100 + r * 10 + e = 10000 * m + o * 1000 + 100 * n + 10 * e + y;

solve satisfy;