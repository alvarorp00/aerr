% Bake cakes --> 2 types of cake {banana, chocolate}

int: p_bc = 400; % profit of a banana cake
int: p_cc = 400; % profit of a chocolate cake

% Commons (quantities)

int: q_rs = 4000; % 4000g == 4kg of self-raising flour
int: q_bs = 6; % 6 bananas
int: q_sg = 2000; % 2000g == 2kg of sugar
int: q_bt = 500; % 500g of butter
int: q_ca = 500; % 500g of cocoa

% Banana cake restrictions

% A banana cake which takes 250g of self-raising flour, 2 mashed bananas, 75g sugar and 100g of butter

var 0..: b;

constraint 2*b <= q_bs;
constraint 250 * b <= q_rs;
constraint 75 * b <= q_sg;
constraint 100 * b <= q_bt;

% Chocolate cake restrictions

% A chocolate cake which takes 200g of self-raising flour, 75g of cocoa, 150g sugar and 150g of butter. 

var 0..: c;

constraint 75 * c <= q_ca;
constraint 200 * c <= q_rs;
constraint 150 * c <= q_sg;
constraint 150 * c <= q_bt;

% Commons (restrictions)

constraint 250*b + 200*c <= q_rs;
%...

solve maximize p_bc * b + p_cc * c;