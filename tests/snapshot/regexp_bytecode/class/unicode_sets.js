// Nested classes
/[ab[cd][e-g]]/v;
/[a[[[d-f]]]]/v;

// Eager inversion of sets
/[^a-z]/v;
/[^\p{ASCII_Hex_Digit}]/v;

// Intersection
/[[a-h]&&[c-n]]/v;
/[^[a-h]&&[c-n]]/v;

// Difference
/[[a-h]--[c-e]]/v;
/[^[a-h]--[c-e]]/v;