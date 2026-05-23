// Shorthand digit and whitespace classes have own instructions
/\s\S/;

// Shorthand as single element of class
/[\s][\S]/;

// Shorthand mixed in class
/[\sa][\S\x0B]/;
/[\sa-z][\S\x0A-\x0C]/;

// Union is the entire unicode range
/[\s\S]/;