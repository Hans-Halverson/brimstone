// Full pattern is a required literal
/abc/;
/foobar/;
/café123/;

// Literal below minimum length
/ab/;

// Only a prefix of long literals is stored
/abcdefghijklmnopqrstuvwxyz/;
/abcdefghijklmnopqrstĀuvwxyz/;

// Literals interrupted by a non-Latin1 code point
/Āabcdefgh/;
/abcĀdefgh/;
/abcdefĀghijkl/;
/aĀĀbbbĀcccc/;

// Literals interrupted before reaching the minimum length
/abĀcd/;

// Count offsets in code points, accounting for unicode mode
/😀foobar/u;
/😀foobar/;

// Use the longest literal if there are multiple
/Āabcdefgh.*xyz/;
/Āabc.*uvwxyz/;

// Non-literal terms break up consecutive literals
/a(?:bcdef)g/;
/abc.def.ghij/;

// Prefer bounded offsets
/(abc)+def/;

// Quantifiers require a literal only if they have a required repetition
/(abcd)+xy/;
/(abcde)?xyz/;
/x{3}foobar/;
/x{2,5}foobar/;
/x+foobar/;
/.*foobar/;

// Quantifiers have a width range
/x{2,5}abĀvwxyz/;

// Character classes consume one code point, except when they may contain strings
/[xy]foobar/;
/[xy]foobar/v;
/[xy\q{zz}]foobar/v;
/\p{ASCII_Hex_Digit}foobar/v;
/\p{Emoji_Keycap_Sequence}foobar/v;

// Wildcards have a width of one
/.foobar/;

// Assertions and lookarounds have no width
/^foobar$/;
/(?=abcdef)xyz/;

// Backreferences have unbounded width
/(\w{3})\1foobar/;

// Disjunctions with multiple alternatives have no required literal
/foobar|bazqux/;
/(?:foo|bar)abcdef/;
/(?:ab|cde)foobar/;

// Case insensitive literals are broken by code points with case variants
/foobar/i;
/123456/i;
/foo_123_bar/i;
/é12345/i;
/×12345/i;

// Modifier groups are accounted for
/(?i:foobar)xyz/;
/(?-i:foobar)xyz/i;
/(?i:error404)xyz/;

// Null bytes are a valid required literal
/\u0000\u0000\u0000/;