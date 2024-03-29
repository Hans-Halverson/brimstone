/(abc)/;
/(a|b)/;

// Anonymous capture group
/(?:abc)/;

// Lookaround
/(?=abc)/;
/(?!abc)/;
/(?<=abc)/;
/(?<!abc)/;

// Named capture group
/(?<name>abc)/;

// Nested capture groups
/(a(?<name>b(c)))/;