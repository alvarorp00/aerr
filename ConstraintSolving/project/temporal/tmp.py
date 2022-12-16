import numpy as np

x = np.int32(-2147483646)
y = np.int32(-1073741824)
z = np.int32(-2147483646)

eq1 = x >= 3*z + 6;
eq2 = x >= 2*z + 6;
eq3 = x <= -3*y + 2;
eq4 = x <= 2*y + 3;

ineq1 = 3*z + 6 <= -3*y + 2;
ineq2 = 3*z + 6 <= 2*y + 3;
ineq3 = 2*z + 6 <= -3*y + 2;
ineq4 = 2*z + 6 <= 2*y + 3;

ineq5 = z <= y + 1;
ineq6 = 2*y <= z + 1;
ineq7 = y <= z + 1;

print(eq1)
print(eq2)
print(eq3)
print(eq4)

print(ineq1)
print(ineq2)
print(ineq3)
print(ineq4)
print(ineq5)
print(ineq6)
print(ineq7)