// Pattern characters which are only allowed in Annex B mode
/]/;
/{/;
/}/;

// Start parsing as a quantifier, but then reparse as pattern on failure
/a{/;
/a{1/;
/a{1,/;
/a{1,2/;
/a{123456789012345678901234567890}/;