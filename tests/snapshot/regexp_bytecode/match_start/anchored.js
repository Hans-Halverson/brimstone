// Simple anchored matches
/^/;
/^/m;

// Modifier groups change the anchor type
/(?m:^)a/;
/(?-m:^)a/m;
/(?m:(?-m:^))a/;

// Anchors mixed between input and line anchored are line anchored
/^a|(?m:^)b/;

// Anchors are found through nested groups
/(?:^a)b/;
/(^)a/;
/((?:^))a/;

// Anchors are found through quantifiers that match at least once
/(?:^)+a/;
/(^a){1,2}b/;

// Anchors in quantifiers that may match zero times are not anchors
/(?:^)?a/;
/(?:^)*a/;

// Anchors after zero-width terms are still anchors
/\b^a/;
/(?=x)^a/;
/(?<=x)^a/;

// Anchors after consuming terms are not anchors
/a^b/;
/a?^b/;
/(?:a)^b/;

// Anchors inside lookarounds are not anchors
/(?=^)a/;

// Anchored patterns take precedence over optional patterns
/^a?/;
/^a*/;

// Anchors survive terms whose first code points cannot be computed
/^.*a/;
/^\1(a)/;

// Anchor must be present in all alternatives
/^a|^b|^c/;
/(?m:^a)|b/;
/^a|b^c/;
/^|a/;
/^a|/;
