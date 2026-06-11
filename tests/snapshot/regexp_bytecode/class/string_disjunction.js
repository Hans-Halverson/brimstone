/[\q{ab}c]/v;

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

// Empty strings
/[\q{}]/v;
/x[\q{|}]y/v;

// Empty string with other strings
/x[\q{abc|}]y/v;
/x[\q{|abc}]y/v;

// Empty string with code points
/x[\q{a|}]y/v;
/x[\q{|a}]y/v;

// Empty string, code points, and strings
/x[\q{|a|abc}]y/v;