// Square root program, verified in Dafny

method sqrt(x: nat) returns (r: nat)
    ensures r * r <= x < (r + 1) * (r + 1)
{
    r := 0;
    while ((r + 1) * (r + 1) <= x)
        invariant r * r <= x
        decreases x - r
    {
        r := r + 1;
    }
}