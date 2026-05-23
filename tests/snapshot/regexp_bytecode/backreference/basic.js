// Correct capture group is referenced
/(a)\1/;
/(a)(b)(c)\3\1\2/;

// Case insensitive backreference
/(a)\1/i;