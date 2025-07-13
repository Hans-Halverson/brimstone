// Basic
/[]/;
/[abc]/;
/[a-z]/;
/[a-zA-ZqR-Z]/;
/[\w\W\s\S\d\D]/;

// Inverted
/[^]/;
/[^abc]/;
/[^^^]/;

// Minus characters
/[-]/;
/[--]/;
/[---]/;
/[a-]/;
/[+--]/;
/[-a]/;
/[--a]/;

// Escape characters
/[\r\n\f\v\t\b]/;
/[\u0061\u0062]/;
/[\u{61}\u{0062}]/u;
/[\cI]/;
/[\\\/\^\$\.\*\+\?\(\)\[\]\{\}\|\:\;\"]/;
/[\\\/\^\$\.\*\+\?\(\)\[\]\{\}\|]/u;
/[\0\x61\x62\xFF]/;

// Unicode properties
/[\p{ASCII}]/u;
/[\P{ASCII_Hex_Digit}]/u;
/[\p{Basic_Emoji}]/v;