// Shorthand word classes expanded
/\w/;
/\W/;

// Shorthand as single element of class
/[\w][\W]/;

// Shorthand mixed in class
/[\wa][\Wa]/;
/[\wa-z][\Wa-z]/;

// Union is the entire unicode range
/[\w\W]/;