// Does not map code points outside Latin1 block
/[a-z]/i;
/\w/i;

// Maps code points outside Latin1 block
/[a-z]/iu;
/\w/iu;

// Inversion
/[^a-z]/i;

// Inversion of word character in unicode and non-unicode modes
/\W/i;
/\W/iu;

// Maps entire unicode properties
/\p{Uppercase}/iu;
/\p{Uppercase}/iv;

// Inversion of unicode properties
/[^\p{Uppercase}]/iu;
/[^\p{Uppercase}]/iv;

// Other shorthand properties
/\s/i;
/\s/iu;
/S/i;
/S/iu;
/\d/i;
/\d/iu;
/D/i;
/D/iu;