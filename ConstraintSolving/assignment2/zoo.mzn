% Minimize buses

var int: kids = 300;

var int: a_cost = 500;
var int: b_cost = 400;

var int: a_kids = 40;
var int: b_kids = 30;

var 0..100: a;
var 0..100: b;

constraint a>=0;
constraint b>=0;

constraint a_kids * a + b_kids * b = kids;

solve minimize a * a_cost + b * b_cost;