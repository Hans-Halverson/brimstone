/a(x)?b/;
/a(x)+b/;
/a(x)*b/;

// Inlined required repetitions
/a(x){3,4}b/;

// Loop required repetitions
/a(x){100,101}b/;

// Inlined optional repetitions
/a(x){1,4}b/;

// Loop optional repetitions
/a(x){1,100}b/;

// Unbounded optional repetitions
/a(x){2,}b/;

// Inlined exact repetitions
/a(x){3}b/;

// Loop exact repetitions
/a(x){100}b/;
