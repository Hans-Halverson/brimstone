// Basic classes
/[]/v;
/[abc]/v;
/[a-z]/v;
/[a-zA-ZqR-Z]/v;
/[\w\W\s\S\d\D]/v;

// Inverted classes
/[^]/v;
/[^abc]/v;

// Escape characters
/[\r\n\f\v\t\b]/v;
/[\u0061\u0062]/v;
/[\u{61}\u{0062}]/v;
/[\cI]/v;
/[\0\x61\x62\xFF]/v;

// Syntax characters
/[\/\-\\^\$\.\*\+\?\(\)\[\]\{\}\|]/v;

// Reserved punctuators
/[\&\-\!\#\%\,\:\;\<\=\>\@\`\~]/v;

// Unicode properties
/[\p{ASCII}]/v;
/[\P{ASCII_Hex_Digit}]/v;
/\p{Basic_Emoji}/v;
/[\p{Basic_Emoji}]/v;

// Intersection
/[\w&&\d]/v;
/[\w&&\d&&\s&&\p{ASCII}]/v;
/[[a-z]&&\d&&[a]]/v;
/[^\w&&\d]/v;

// Difference
/[\w--\d]/v;
/[\w--\d--\s--\p{ASCII}]/v;
/[[a-z]--\d--[a]]/v;
/[^\w--\d]/v;

// Nested classes
/[[a-z]]/v;
/[[[a-z]]]/v;
/[[[]]]/v;
/[[[\w&&\s]--[a-z]]abcd-e]/v;
