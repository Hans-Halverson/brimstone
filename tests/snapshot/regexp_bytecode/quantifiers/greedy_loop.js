// Literal greedy loop bodies
/ax*b/;
/ax*b/i;

// Character class greedy loop bodies
/a[a-z]*b/;
/a[^a-z]*b/;
/a[^a-z]*b/v;

// Wildcard greedy loop bodies
/a.*b/;
/a.*b/s;

// Descend into simple anonymous groups
/a(?:x)*b/;
/a(?:(?:(?:x)))*b/;

// Greedy loop can appear after required repetitions
/ax+/;
/ax{100,}b/;

// Not greedy loop bodies
/ax*?b/;
/a(?:xy)*b/;
/a(x)*b/;
/a(?i:x)*b/;