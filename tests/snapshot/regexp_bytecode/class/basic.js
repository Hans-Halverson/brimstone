// Single element class. Is simplified to literal.
/[a]/;

// Inverted single element class cannot be simplified to literal.
/[^a]/;

// Multiple element class
/[ac]/;

// Range class
/[a-z]/;

// Multiple ranges
/[a-z0-9AC]/;

// Cheaper inverted class
/[\u{100}-\u{200}\u{300}-\u{10FFFF}]/u;