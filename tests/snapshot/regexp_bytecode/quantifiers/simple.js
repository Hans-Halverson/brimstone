/ax?b/;
/ax+b/;
/ax*b/;

// Inlined required repetitions
/ax{3,4}b/;

// Loop required repetitions
/ax{100,101}b/;

// Inlined optional repetitions
/ax{1,4}b/;

// Loop optional repetitions
/ax{1,100}b/;

// Unbounded optional repetitions
/ax{2,}b/;

// Inlined exact repetitions
/ax{3}b/;

// Loop exact repetitions
/ax{100}b/;

// Too many repetitions
/ax{0,1000000000000}b/;
/a(?:x|){0,1000000000000}b/;
/ax{1000000000000,1000000000001}b/;
/a(?:x|){1000000000000,1000000000001}b/;