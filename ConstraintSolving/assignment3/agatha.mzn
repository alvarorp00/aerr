% Who killed agatha?

% Disclaimer: it's wrong

enum People = {Agatha, Butler, Charles};

array[int] of People: h1 = [Charles];  % Hatreds of Agatha
array[int] of People: h2 = [Agatha, Butler];  % Hatreds of Charles
array[int] of People: h3 = [Charles]; % Hatreds of Butler

var People: killer;
var People: victim = Agatha;

constraint killer in h3;  % Butler hates everyone richer than Agatha
constraint if killer == Agatha then victim in h1 endif;
constraint if killer == Charles then victim in h2 endif;
constraint if killer == Butler then victim in h3 endif;