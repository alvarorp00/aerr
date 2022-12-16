% Nodes with no consecutive values

enum Nodes = {node1, node2, node3, node4, node5, node6, node7, node8};

set of int: Identifiers = 1..8;

array[Nodes] of var Identifiers: id; % identifiers of each node

include "alldifferent.mzn";
constraint alldifferent(id);

% Constraints with no repetition --> if constraint added previously then it's not set again

% Node1
constraint abs(id[node1] - id[node2]) != 1;
constraint abs(id[node1] - id[node3]) != 1;
constraint abs(id[node1] - id[node4]) != 1;

% Node2

constraint abs(id[node2] - id[node3]) != 1;
constraint abs(id[node2] - id[node5]) != 1;
constraint abs(id[node2] - id[node6]) != 1;

% Node 3

constraint abs(id[node3] - id[node4]) != 1;
constraint abs(id[node3] - id[node6]) != 1;
constraint abs(id[node3] - id[node7]) != 1;

% Node 4

constraint abs(id[node4] - id[node6]) != 1;
constraint abs(id[node4] - id[node7]) != 1;

% Node 5

constraint abs(id[node5] - id[node6]) != 1;
constraint abs(id[node5] - id[node8]) != 1;

% Node 6

constraint abs(id[node6] - id[node7]) != 1;
constraint abs(id[node6] - id[node8]) != 1;

% Node 7

constraint abs(id[node7] - id[node8]) != 1;

solve satisfy;
