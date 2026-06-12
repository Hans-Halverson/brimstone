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

// Quantifier, not a unicode escape in Annex B
/a\u{2}b/;

// Bounds too large to represent saturate to u64::MAX instead of erroring
/a{99999999999999999999}/;
/a{99999999999999999999,}/;
/a{1,99999999999999999999}/;
/a{99999999999999999999,99999999999999999999}/;