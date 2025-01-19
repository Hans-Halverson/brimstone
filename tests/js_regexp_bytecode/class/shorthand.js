// Shorthand digit and whitespace classes have own instructions
/\d\D\s\S/;

// Shorthand as single element of class
/[\d][\D][\s][\S]/;

// Shorthand mixed in class
/[\da][\Da][\sa][\Sa]/;
/[\da-z][\Da-z][\sa-z][\Sa-z]/