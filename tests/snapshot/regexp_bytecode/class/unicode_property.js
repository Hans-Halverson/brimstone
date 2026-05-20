// Escape outside class
/\p{ASCII_Hex_Digit}/u;

// Escape inside class
/[\p{ASCII_Hex_Digit}]/u;

// Escape mixed in class
/[\p{ASCII_Hex_Digit}1]/u;
/[\p{ASCII_Hex_Digit}1-9]/u;

// Complement class
/\P{ASCII_Hex_Digit}/u;
/[\P{ASCII_Hex_Digit}C]/u;

// Scripts
/\p{Script=Latin}/u;

// General category groups
/\p{General_Category=Separator}/u;

// Inverted property
/[^\p{ASCII_Hex_Digit}]/u;