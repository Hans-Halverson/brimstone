a`text`;
a.b`text`;
a + b`text`;

// Malformed escape sequences are allowed
a`text\01${2}foo\uX${3}allowed`;