enum Men = {Abel, Locker, Snyder};
enum Women = {Edith, Doris, Luisa};
enum Children = {Albert, Henry, Victor};

set of Women: Wifes = {Edith, Doris, Luisa};
set of Men: Husbands = {Abel, Locker, Snyder};

set of Men: Fathers = {Abel, Locker, Snyder};
set of Women: Mothers = {Edith, Doris, Luisa};

array[Men] of var Wifes: wife;
% array[Women] of var Husbands: husband;
array[Children] of var Fathers: father;
array[Children] of var Mothers: mother;

include "alldifferent.mzn";

constraint alldifferent(wife);
% constraint alldifferent(husband);
constraint alldifferent(father);
constraint alldifferent(mother);

constraint wife[Snyder] != Luisa;
constraint wife[father[Henry]] != Luisa;

constraint wife[Locker] != Edith;
constraint mother[Albert] != Edith;

constraint if father[Albert] == Locker \/ father[Albert] == Snyder then mother[Victor] == Luisa endif;

constraint if wife[Locker] == Luisa then mother[Albert] != Doris endif;

constraint forall(c in Children)(
               mother[c] == wife[father[c]]
           );

solve satisfy;
