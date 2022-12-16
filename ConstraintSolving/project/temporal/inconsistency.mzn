% Analyze the consistency of the following system of linear inequalities using the algorithm of Fourier/Motzkin:

% Inequations:
% x + z <= x + < + 1
% y +3z +6 ≤ x + y
% −y +2z + 6 ≤ x − y
% x + y ≤ −2y + 2
% x +z ≤ 2y +z +3
% x +2y ≤ x +z +1
% x + y ≤ x +z +1

% Variables:

var int: y;  % we just need to know if y is integer or not as the other two (x, z) are going to be encapsulated in the inequalities under variable y (see below)

% Inequations:

% This are the inequalities we want to analyze:
% INEQ1 = x + z <= x + y + 1;
% INEQ2 = y + 3*z + 6 <= x + y;
% INEQ3 = -y + 2*z + 6 <= x - y;
% INEQ4 = x + y <= -2*y + 2;
% INEQ5 = x + z <= 2*y + z + 3;
% INEQ6 = x + 2*y <= x + z + 1;
% INEQ7 = x + y <= x + z + 1;

% Solve the system of inequalities:

% Fourier/Motzkin:

% Ref.: https://techfirst.medium.com/fourier-motzkin-elimination-a7f668f3d695

% Involving inequalities 2,3,4,5 we end up with a collection of lower and upper bounds for x:
% x in function of y:
% -> x <= -3*y + 2;  % X's upper bound is now -3*y + 2 (ineq.4) [UPPER BOUND 1]
% -> x <= 2*y + 3;  % X's upper bound is now 2*y + 3 (ineq.5) [UPPER BOUND 2]
% x in function of z:
% -> x >= 3*z + 6;  % X's lower bound is now 3*z + 6 (ineq.2) [LOWER BOUND 1]
% -> x >= 2*z + 6;  % X's lower bound is now 2*z + 6 (ineq.3) [LOWER BOUND 2]
% Now we write down the new inequalities (k lower bounds and l upper bounds, so we have k * l new inequalities; k=2, l=2):

% INEQ1', INEQ6', INEQ7' are the same as they don't involve x, although we reduce them to delete the variable x (which appears in both sides of the inequality and can be removed):

% New inequalities without the variable x:
% INEQ 1' = z <= y + 1;
% INEQ 2' = 3*z + 6 <= -3*y + 2;  (lower bound 1 of x <= upper bound 1 of x)
% INEQ 3' = 3*z + 6 <= 2*y + 3;  (lower bound 1 of x <= upper bound 2 of x)
% INEQ 4' = 2*z + 6 <= -3*y + 2;  (lower bound 2 of x <= upper bound 1 of x)
% INEQ 5' = 2*z + 6 <= 2*y + 3;  (lower bound 2 of x <= upper bound 2 of x)
% INEQ 6' = 2*y <= z + 1;
% INEQ 7' = y - 1 <= z;

% Involving above inequalities we end up with a collection of lower and upper bounds for z:
% z in function of y:
% -> z <= y + 1; [UPPER BOUND 1]
% -> z <= -y - 4/3; [UPPER BOUND 2]
% -> z <= (2*y - 3)/3; [UPPER BOUND 3]
% -> z <= (-3*y - 4)/2; [UPPER BOUND 4]
% -> z <= y - 3/2; [UPPER BOUND 5]
% -> z >= 2 * y - 1; [LOWER BOUND 1]
% -> z >= y - 1; [LOWER BOUND 2]
% Now we write down the new inequalities (k lower bounds and l upper bounds, so we have k * l new inequalities; k=2, l=5):

% constraint 2*y - 1 <= y + 1; (lower bound 1 of z <= upper bound 1 of z)
% constraint 2*y - 1 <= -y - 4/3; (lower bound 1 of z <= upper bound 2 of z)
% constraint 2*y - 1 <= (2*y - 3)/3; (lower bound 1 of z <= upper bound 3 of z)
% constraint 2*y - 1 <= (-3*y - 4)/2; (lower bound 1 of z <= upper bound 4 of z)
% constraint 2*y - 1 <= y - 3/2; (lower bound 1 of z <= upper bound 5 of z)

% constraint y - 1 <= y + 1; (lower bound 2 of z <= upper bound 1 of z)
% constraint y - 1 <= -y - 4/3; (lower bound 2 of z <= upper bound 2 of z)
% constraint y - 1 <= (2*y - 3)/3; (lower bound 2 of z <= upper bound 3 of z)
% constraint y - 1 <= (-3*y - 4)/2; (lower bound 2 of z <= upper bound 4 of z)
% constraint y - 1 <= y - 3/2; (lower bound 2 of z <= upper bound 5 of z)

% Rewrite above constraints so they're more readable:

% constraint y <= 2;
% constraint 3*y <= -1/3;
% constraint 2*y <= (2*y)/3;  % same
% constraint 2*y <= (-3*y - 2) / 2;  % same
% constraint 2*y <= y - 1/2;

% constraint y <= y + 2; % trivial
% constraint y <= (- 1/3) / 2;
% constraint y <= 0;
% constraint y <= (-3*y - 2)/2;
% constraint y - 1 <= y - 3/2;

% Rewrite above to be int-compatible (float-type operations can be very slow to compute):

constraint y <= 2;
constraint 9*y <= -1;
constraint 6*y <= 2*y;
constraint 4*y <= -3*y - 2;
constraint 4*y <= 2*y - 1;

constraint y <= y + 2;
constraint 2*y <= -1/3;
constraint y <= 0;
constraint 2*y <= -3*y - 2;
constraint 2*y <= 2*y - 1;

% Solve the system of inequalities:
solve satisfy;