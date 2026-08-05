// Simple literals
/a/;
/abc/;

// Case insensitive literals include their case closure.
/a/i;
/k/i;
/k/iu;

// Emoji literals in unicode and non-unicode mode
/😀/u;
/😀/;

// Alternatives union their first code points
/ab|cd/;
/a|b|c/;
/(ab|cd)e/;

// Optional terms union their first code points with later terms
/a?b/;
/a*b/;
/a+b/;
/a{0}b/;
/a{2}b/;
/(?:ab)?cd/;
/(a|)b/;
/(?:a|b?)c/;

// Fully optional patterns have an unknown start
/a?/;
/a*/;
/a|/;
/(?:)/;
/(?=a)/;

// Wildcards have an unknown start
/.a/;
/a|./;

// Backreferences have an unknown start, but earlier consuming terms are used
/(a)\1/;
/\1(a)/;

// Zero-width terms are skipped when finding first code points
/\bfoo/;
/(?=x)foo/;
/(?!x)foo/;
/(?<=x)foo/;

// Character classes
/[abc]x/;
/[a-fh]/;
/[a-c]/i;
/\d+/;
/\w/;

// Inverted character classes use the complement
/[^a]x/;
/[^]/;

// Empty character classes are conservatively treated as optional
/[]x/;

// Modifier groups change the flags for first code points
/(?i:a)b/;
/a(?i:b)/;
/(?i:a?)b/;

// Unicode sets mode string disjunctions add their first code points
/[\q{abc|de}x]/v;
/[\q{ab}]/v;
/[\q{ab}]/vi;

// Empty strings in string disjunctions make the class optional
/[\q{}a]x/v;
/[\q{}]/v;

// Unicode sets mode set operations
/[[a-c]&&[b-d]]x/v;
/[[a-d]--[b]]/v;
/[^a]/v;
