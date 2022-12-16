% Space of 4x3 to be filled with two L and one O figures

% Board

int: x_lim = 4;
int: y_lim = 4;

set of int: COLUMNS = 1..x_lim;
set of int: ROWS = 1..y_lim;

% Figures

enum Figure = {L,S,I};
enum Block = {L1,L2,S1,I1};

array[Block] of Figure: BlockType = [L,L,S,I];

% blocktype constraints

constraint BlockType[L1] = L;
constraint BlockType[L2] = L;
constraint BlockType[S1] = S;
constraint BlockType[I1] = I;

% end

% set of Block: BLOCKS = {L1,L2,S1,I1};
set of Block: BLOCKS = {S1,I1,L1,L2};

array[ROWS, COLUMNS] of var BLOCKS: board;  % the idea is to distinguish between the blocks of the board (throught the block number); as 0 is not allowed, then we want to fill the entire board

array[Block] of var COLUMNS: x;  % x coordinate of the block
array[Block] of var ROWS: y;  % y coordinate of the block

% Valid coordinates for each block

% constraint forall(b in BLOCKS)(
%     if BlockType[b] = L then
%         x[b] in 1..x_lim-2 /\ y[b] in 2..y_lim
%     elseif BlockType[b] = S then
%         x[b] in 1..x_lim-2 /\ y[b] in 1..y_lim-1
%     elseif BlockType[b] = I then
%         x[b] in 1..x_lim-3 /\ y[b] in 1..y_lim
%     else
%         true  % should never happen
%     endif
% );

constraint (board[y[I1], x[I1]] = I1 /\ board[y[I1], x[I1]+1] = I1 /\ board[y[I1], x[I1]+2] = I1 /\ board[y[I1], x[I1]+3] = I1) \/
            (board[y[I1], x[I1]] = I1 /\ board[y[I1]+1, x[I1]] = I1 /\ board[y[I1]+2, x[I1]] = I1 /\ board[y[I1]+3, x[I1]] = I1);

constraint (board[y[L1], x[L1]] = L1 /\ board[y[L1], x[L1]+1] = L1 /\ board[y[L1], x[L1]+2] = L1 /\ board[y[L1]-1, x[L1]+2] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1]+1, x[L1]] = L1 /\ board[y[L1]+2, x[L1]] = L1 /\ board[y[L1]+2, x[L1]+1] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1]-1, x[L1]] = L1 /\ board[y[L1]-2, x[L1]] = L1 /\ board[y[L1]-2, x[L1]-1] = L1) \/
            (board[y[L1], x[L1]] = L1 /\ board[y[L1], x[L1]-1] = L1 /\ board[y[L1], x[L1]-2] = L1 /\ board[y[L1]+1, x[L1]-2] = L1);

constraint (board[y[L2], x[L2]] = L2 /\ board[y[L2], x[L2]+1] = L2 /\ board[y[L2], x[L2]+2] = L2 /\ board[y[L2]-1, x[L2]+2] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2]+1, x[L2]] = L2 /\ board[y[L2]+2, x[L2]] = L2 /\ board[y[L2]+2, x[L2]+1] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2]-1, x[L2]] = L2 /\ board[y[L2]-2, x[L2]] = L2 /\ board[y[L2]-2, x[L2]-1] = L2) \/
            (board[y[L2], x[L2]] = L2 /\ board[y[L2], x[L2]-1] = L2 /\ board[y[L2], x[L2]-2] = L2 /\ board[y[L2]+1, x[L2]-2] = L2);

constraint (board[y[S1], x[S1]] = S1 /\ board[y[S1], x[S1]+1] = S1 /\ board[y[S1]+1, x[S1]+1] = S1 /\ board[y[S1]+1, x[S1]+2] = S1) \/
            (board[y[S1], x[S1]] = S1 /\ board[y[S1]+1, x[S1]] = S1 /\ board[y[S1]+1, x[S1]+1] = S1 /\ board[y[S1]+2, x[S1]+1] = S1);

% constraint forall(b in BLOCKS)(
%     if BlockType[b] = L then
%         (board[y[b], x[b]] = b /\ board[y[b], x[b]+1] = b /\ board[y[b], x[b]+2] = b /\ board[y[b]-1, x[b]+2] = b) \/
%         (board[y[b], x[b]] = b /\ board[y[b]+1, x[b]] = b /\ board[y[b]+2, x[b]] = b /\ board[y[b]+2, x[b]+1] = b) \/
%         (board[y[b], x[b]] = b /\ board[y[b]-1, x[b]] = b /\ board[y[b]-2, x[b]] = b /\ board[y[b]-2, x[b]-1] = b) \/
%         (board[y[b], x[b]] = b /\ board[y[b], x[b]-1] = b /\ board[y[b], x[b]-2] = b /\ board[y[b]+1, x[b]-2] = b)
%     elseif BlockType[b] = S then
%         (board[y[b], x[b]] = b /\ board[y[b], x[b]+1] = b /\ board[y[b]+1, x[b]+1] = b /\ board[y[b]+1, x[b]+2] = b) \/
%         (board[y[b], x[b]] = b /\ board[y[b]-1, x[b]] = b /\ board[y[b]-1, x[b]+1] = b /\ board[y[b]-2, x[b]+1] = b)
%     elseif BlockType[b] = I then
%         (board[y[b], x[b]] = b /\ board[y[b], x[b]+1] = b /\ board[y[b], x[b]+2] = b /\ board[y[b], x[b]+3] = b) \/
%         (board[y[b], x[b]] = b /\ board[y[b]+1, x[b]] = b /\ board[y[b]+2, x[b]] = b /\ board[y[b]+3, x[b]] = b)
%     else
%         true  % should never happen
%     endif
% );

% Each block does not overlap

% constraint forall(b in BLOCKS)(
%     forall(i in ROWS, j in COLUMNS)(
%         if board[i,j] = b then
%             forall(k in ROWS, l in COLUMNS)(
%                 if k = i /\ l = j then
%                     board[k,l] = b
%                 endif
%             )
%         endif
%     )
% );

% Block is not placed more times than allowed

constraint forall(b in BLOCKS)(
    count(i in ROWS, j in COLUMNS)(board[i,j] = b) = 4
);

% Solve
ann: search_ann;
search_ann = int_search(board, first_fail, indomain_median, complete);
solve :: search_ann satisfy;

% Show board (4x4 grid)
output ["Board: [" ++ "\n" ++
             " " ++ show(board[1,1]) ++ " " ++  show(board[1,2]) ++ " " ++  show(board[1,3]) ++ " " ++  show(board[1,4]) ++ "\n"
            ++ " " ++  show(board[2,1]) ++ " " ++  show(board[2,2]) ++ " " ++  show(board[2,3]) ++ " " ++ show(board[2,4]) ++ "\n"
            ++ " " ++  show(board[3,1]) ++ " " ++  show(board[3,2]) ++ " " ++  show(board[3,3]) ++ " " ++ show(board[3,4]) ++ "\n"
            ++ " " ++  show(board[4,1]) ++ " " ++  show(board[4,2]) ++ " " ++  show(board[4,3]) ++ " " ++ show(board[4,4]) ++ "\n" ++ "]"];