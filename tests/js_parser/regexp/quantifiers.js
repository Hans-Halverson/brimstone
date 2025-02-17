/a*/;
/a+/;
/a?/;
/a{1}/;
/a{123,}/;
/a{1,2}/;

// Lazy quantifiers
/a*?/;
/a+?/;
/a??/;
/a{1}?/;
/a{1,}?/;
/a{1,2}?/;

// Overflowing max quantifier bound which is treated as having no max bound
/a{1,9876543219876543219876543219876543219876543219876543219876543219876543219876543219876543219876543219876543210}/;