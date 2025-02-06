/[\q{ab}c]/v;
/[\q{}]/v;

// Multiple alternatives
/[\q{ab|abcde|abc}]/v;

// Single code point strings are treated as single code points
/[\q{a|b|c|de}]/v;

// Unions
/[\q{aa|bb}\q{cc}[\q{dd}]]/v;

// Intersection
/[\q{aa|bb}&&\q{bb|cc}]/v;

// Subtraction
/[\q{aa|bb}--\q{aa}]/v;