// Escape outside class
/\p{Letter}/u;

// Escape inside class
/[\p{Letter}]/u;

// Escape mixed in class
/[\p{Letter}1]/u;
/[\p{Letter}1-9]/u;
