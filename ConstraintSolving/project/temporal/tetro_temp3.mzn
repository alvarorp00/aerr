% Space of 4x3 to be filled with two L and one O figures:
%  - Rows: 1..3
%  - Columns: 1..4

% Board

int: x_lim = 4;
int: y_lim = 4;

set of int: COLUMNS = 1..x_lim;
set of int: ROWS = 1..y_lim;

% Figures

int : L = 1;
int : S = 2;
int : I = 3;

int : L1 = 1;
int : L2 = 2;
int : S1 = 3;
int : I1 = 4;

set of int: FIGURES = 1..3;  % L,S,I
set of int: BLOCKS = 1..4;  % L1,L2,S1,I1

% Figures
array[BLOCKS] of var FIGURES: BlockType = [L1: L, L2: L, S1: S, I1: I];

% end

array[COLUMNS, ROWS] of var BLOCKS: board;  % the idea is to distinguish between the blocks of the board (throught the block number); as 0 is not allowed, then we want to fill the entire board

array[BLOCKS] of var COLUMNS: x;  % x coordinate of the block
array[BLOCKS] of var ROWS: y;  % y coordinate of the block

% Constraints

% constraint x[I1] = 1 \/ x[I1] = 4;  % I1 must be on the left or right side of the board as it is the only I figure
% constraint y[I1] = 1 \/ y[I1] = 4;  % I1 must be on the top or bottom of the board as it is the only I figure

% We fix the position of the I1 figure either on the left or right side and on the top or bottom of the board

% By symmetry, we can fix the position of the I1 figure on the left or right side and on the top or bottom of the board,
% as if we fix it somewhere else then we won't have enough space to put the other figures (L1,L2,S1 won't fit on the empty row/column left by the I1 figure)
% --> We fix it on the top of the board
% var bool: I1_top = (board[1,1] = I1 /\ board[2,1] = I1 /\ board[3,1] = I1 /\ board[4,1] = I1) /\ % I1 on the top
%                     (x[I1] = 1) /\ % I1 first square on the left
%                     (y[I1] = 1);  % I1 on the top (y[I1] = 1 is redundant but it is useful to have it in the model to see the symmetry of the problem [see the comment above])

var bool: I1_top = (x[I1] = 1) /\ % I1 first square on the left
                    (y[I1] = 1) /\  % I1 on the top (useful to have it in the model to see the symmetry of the problem)
                    (board[x[I1],y[I1]] = I1 /\ board[x[I1]+1,y[I1]] = I1 /\ board[x[I1]+2,y[I1]] = I1 /\ board[x[I1]+3,y[I1]] = I1); % I1 on the top

% var bool: I1_left = (board[1,1] = I1 /\ board[1,2] = I1 /\ board[1,3] = I1 /\ board[1,4] = I1); % I1 on the left
% var bool: I1_bottom = (board[1,4] = I1 /\ board[2,4] = I1 /\ board[3,4] = I1 /\ board[4,4] = I1); % I1 on the bottom
% var bool: I1_right = (board[4,1] = I1 /\ board[4,2] = I1 /\ board[4,3] = I1 /\ board[4,4] = I1); % I1 on the right

% At least one of the I1 constraints must be true
% constraint I1_top \/ I1_left \/ I1_bottom \/ I1_right;

% Since we've fix it in the top:
constraint I1_top;  % This reduces the search space (we don't need to search for the other I1 constraints)

% constraint if I1_top then y[L1] >= 2 /\ y[L2] >= 2 /\ y[S1] >= 2 % L1,L2,S1 can't be on the top of the board
%             elseif I1_left then x[L1] >= 2 /\ x[L2] >= 2 /\ x[S1] >= 2 % L1,L2,S1 can't be on the left of the board
%             elseif I1_bottom then y[L1] <= 3 /\ y[L2] <= 3 /\ y[S1] <= 3 % L1,L2,S1 can't be on the bottom of the board
%             else x[L1] <= 3 /\ x[L2] <= 3 /\ x[S1] <= 3 % L1,L2,S1 can't be on the right of the board
%             endif;

% L1,L2,S1 can't be on the top of the board
constraint y[L1] >= 2 /\ y[L2] >= 2 /\ y[S1] >= 2;

% Now we solve a 4x3 board with two L and one S figures (the I figure is fixed either on the left or right side and on the top or bottom of the board)
% The idea is to use the board array to distinguish between the blocks of the board (throught the block number); as 0 is not allowed, then we want to fill the entire board

constraint forall(b in BLOCKS)(
    if BlockType[b] = L then
        (board[x[b], y[b]] = b /\ board[x[b], y[b]+1] = b /\ board[x[b], y[b]+2] = b /\ board[x[b]+1, y[b]+2] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b]+1, y[b]] = b /\ board[x[b]+2, y[b]] = b /\ board[x[b]+2, y[b]-1] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b]-1, y[b]] = b /\ board[x[b]-2, y[b]] = b /\ board[x[b]-2, y[b]+1] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b], y[b]-1] = b /\ board[x[b], y[b]-2] = b /\ board[x[b]-1, y[b]-2] = b)
    elseif BlockType[b] = S then
        (board[x[b], y[b]] = b /\ board[x[b]+1, y[b]] = b /\ board[x[b]+1, y[b]+1] = b /\ board[x[b]+2, y[b]+1] = b) \/
        (board[x[b], y[b]] = b /\ board[x[b], y[b]-1] = b /\ board[x[b]+1, y[b]-1] = b /\ board[x[b]+1, y[b]-2] = b)
    else % BlockType[b] = I
        true % I1 constraints are fixed (see above)
    endif
);

% Blocks are not placed twice
constraint forall(b in BLOCKS)(
    sum(i in 1..4, j in 1..4)(board[i,j] = b) = 4  % 4-sized blocks
);

% ann: search_ann;
% search_ann = int_search(board, first_fail, indomain_min);
% solve :: search_ann :: restart_linear(100) satisfy;

solve satisfy;

% Show board (4x4 grid)
output ["Board: [" ++ "\n" ++
            " " ++ show(board[1,1]) ++ " " ++ show(board[2,1]) ++ " " ++ show(board[3,1]) ++ " " ++ show(board[4,1]) ++ "\n" ++
            " " ++ show(board[1,2]) ++ " " ++ show(board[2,2]) ++ " " ++ show(board[3,2]) ++ " " ++ show(board[4,2]) ++ "\n" ++
            " " ++ show(board[1,3]) ++ " " ++ show(board[2,3]) ++ " " ++ show(board[3,3]) ++ " " ++ show(board[4,3]) ++ "\n" ++
            " " ++ show(board[1,4]) ++ " " ++ show(board[2,4]) ++ " " ++ show(board[3,4]) ++ " " ++ show(board[4,4]) ++ "\n]" ++ "\n"];