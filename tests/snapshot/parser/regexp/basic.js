/a/;
/a/img;

// Can use `=` at start of regexp
/=/;
/=abc/;

// Alternation
/a|b/;
/ab|ba|cd/;
/|a|/;
/|||/;

// Assertions and character classes
/^\b\B$/;
/\w\W\s\S\d\D/;

// Backreferences
/()()()()()()()()()()\1\2\3\4\5\6\7\8\9\10/;
/(?<foo>)(?<a>)\k<foo>\k<\u0061>/;