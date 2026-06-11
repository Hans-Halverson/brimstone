/*---
description: Cases where unpaired surrogates in character classes were ignored.
---*/

// A character class containing a single unpaired surrogate
assert(/[\uD83D]/.test('\uD83D'));
assert(/[\uD83D]/u.test('\uD83D'));

// Case insensitive closures that explicitly include unpaired surrogates
assert(/[\uD83D]/i.test("\uD83D"));
assert(/[\uD800-\uDFFF]/i.test("\uDABC"));
assert(/[a\uD83D]/i.test("\uD83D"));

assert(/[\uD83D]/iu.test("\uD83D"));
assert(/[\uD800-\uDFFF]/iu.test("\uDABC"));
assert(/[a\uD83D]/iu.test("\uD83D"));

// Complemented case insensitive closures implicitly include unpaired surrogates
assert.sameValue(/\S/i.exec("\uD83Dx")[0], "\uD83D");
assert.sameValue(/\S/i.exec("\uD83Dx").index, 0);
assert.sameValue(/\D\S/i.exec("\uD83Dx")[0], "\uD83Dx");
assert.sameValue(/\D\S/i.exec("\uD83Dx").index, 0);

assert(/\W/i.test("\uD800"));
assert(/[\D]/i.test("\uD83D"));
assert(/\P{L}/iu.test("\uD83D"));

// MaybeSimpleCase (unicode sets mode only) preserves unpaired surrogates
assert(/[\uD83D]/iv.test("\uD83D"));
assert(/[\uD800-\uDFFF]/iv.test("\uDABC"));
assert(/[\q{\uD83D}]/iv.test("\uD83D"));
assert(/[[\uD83D]]/iv.test("\uD83D"));

// Complement set in unicode sets mode implicitly includes unpaired surrogates
assert(/\P{L}/iv.test("\uD83D"));
assert(/\S/iv.test("\uD83D"));
assert(/[\D]/iv.test("\uD83D"));
