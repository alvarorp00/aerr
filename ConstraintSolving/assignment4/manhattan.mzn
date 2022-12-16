% Imagine placing the numbers 1 to n in different positions in an n × n grid such 
% that the Manhattan distance between any two numbers i and j is greater than the maximum of the two numbers minus 1.
% --> The aim is to minimize the total of the Manhattan distances between the pairs.

% The Manhattan distance between two points (x1, y1) and (x2, y2) is |x1 - x2| + |y1 - y2|.
% The Manhattan distance between two numbers i and j is the Manhattan distance between the positions of i and j in the grid.
% i and j represent the numbers 1 to n.

int: N = 4;

function var int: manhattan_distance(var int: x1, var int: y1, var int: x2, var int: y2) = abs(x1 - x2) + abs(y1 - y2);

set of int: numbers = 1..N;

array[numbers] of var numbers: x_pos;
array[numbers] of var numbers: y_pos;
array [numbers, numbers] of var 0..2*N-2: distances =
    array2d(numbers, numbers, [if i < j then manhattan_distance(x_pos[i], y_pos[i], x_pos[j], y_pos[j]) else 0 endif | i in numbers, j in numbers]);

constraint forall(i in numbers, j in numbers where i < j)(distances[i,j] <= max(x_pos[i], y_pos[j]) - 1);
solve satisfy;