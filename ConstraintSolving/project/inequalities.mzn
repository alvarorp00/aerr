% DEPRECATED: This is a deprecated version of the problem. Please use the new version instead (inequalities.mzn).



% Analyze the consistency of the following system of linear inequalities using the algorithm of Fourier/Motzkin:

% Inequalities:

% INEQ1 = x + z <= x + y + 1;
% INEQ2 = y + 3*z + 6 <= x + y;
% INEQ3 = -y + 2*z + 6 <= x - y;
% INEQ4 = x + y <= -2*y + 2;
% INEQ5 = x + z <= 2*y + z + 3;
% INEQ6 = x + 2*y <= x + z + 1;
% INEQ7 = x + y <= x + z + 1;

% var int: x;  % We don't need to define x as we will define it through its lower and upper bounds
var int: y;
% var int: z; % We don't need to define z as we will define it through its lower and upper bounds

% With below we can see the lower and upper bounds of the variable x:
% Ref.: https://techfirst.medium.com/fourier-motzkin-elimination-a7f668f3d695

% Involving inequalities 2,3,4,5 we end up with a collection of lower and upper bounds for x:
% x in function of y:
% -> x <= -3*y + 2;  % X's upper bound is now -3*y + 2 (ineq.4) [UPPER BOUND 1]
% -> x <= 2*y + 3;  % X's upper bound is now 2*y + 3 (ineq.5) [UPPER BOUND 2]
% x in function of z:
% -> x >= 3*z + 6;  % X's lower bound is now 3*z + 6 (ineq.2) [LOWER BOUND 1]
% -> x >= 2*z + 6;  % X's lower bound is now 2*z + 6 (ineq.3) [LOWER BOUND 2]
% Now we write down the new inequalities (k lower bounds and l upper bounds, so we have k * l new inequalities; k=2, l=2 --> 4 inequalities):

% Involving inequalities 1,6,7 we end up with a collection of lower and upper bounds for z:
% z in function of y:
% -> z <= y + 1;  [UPPER BOUND 1]
% -> z >= 2*y + 1; [LOWER BOUND 1]
% -> z >= y - 1; [LOWER BOUND 2]
% Now we write down the new inequalities (k lower bounds and l upper bounds, so we have k * l new inequalities; k=2, l=1 --> 2 inequalities):

array[1..2] of var int: z_lower_bounds = [2 * y + 1, y - 1];
array[1..1] of var int: z_upper_bounds = [y + 1];

array[1..2] of var int: x_lower_bounds = [3 * max(z_upper_bounds) + 6, 2 * max(z_upper_bounds) + 6];
array[1..2] of var int: x_upper_bounds = [-3 * y + 2, 2 * y + 3];

% We can now solve the system of inequalities:

constraint min(x_upper_bounds) >= max(x_lower_bounds);
constraint min(z_upper_bounds) >= max(z_lower_bounds);

% It took me 45 seconds to solve it through miniZinc IDE (GeCode solver v.6.3.0)
% System info:
% - CPU: 11th Gen Intel(R) Core(TM) i7-1165G7 @ 2.80GHz
% - RAM: 16 GB
% - OS: Manjaro Linux 22.0.0

solve satisfy;  % I tried with other different strategies but all of them run for 45s with no advantage