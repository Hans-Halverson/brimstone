// Single code point
/a/;
/Ā/;

// One byte string literals
/ab/;
/abc/;

// Two byte string literals
/🙂/;
/abcĀabc/;
/abc\uD800def/;

// Unpaired surrogates split literals in unicode modes
/abc\uD800def\uDFFFghi/u;
/abc\uD800def\uDFFFghi/v;
/---\uD800---\uDFFF---/iu;

// Code points with a non-unitary case closure split literals in case insensitive mode
/---a---/i;