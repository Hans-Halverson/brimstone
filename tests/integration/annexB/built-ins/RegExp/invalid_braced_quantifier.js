/*---
description: Braced quantifiers must follow quantifiable terms in Annex B non-unicode mode.
---*/

// Quantifiers at the start of a term
assert.throws(SyntaxError, () => new RegExp("{3}"));
assert.throws(SyntaxError, () => new RegExp("({3})"));
assert.throws(SyntaxError, () => new RegExp("a|{3}"));

// Still errors for invalid quantifier bounds
assert.throws(SyntaxError, () => new RegExp("{5,3}"));

// Lookahead is quantifiable
assert(new RegExp("(?=a){2}").test("a"));
assert(new RegExp("(?!=a){2}").test("b"));

// Quantifiers on all non-quantifiable terms
const nonQuantifiableTerms = ["^", "$", "\\b", "\\B", "(?<=a)", "(?<!a)"];
const bracedQuantifiers = ["{2}", "{2,}", "{2,4}"];

for (const term of nonQuantifiableTerms) {
  for (const quantifier of bracedQuantifiers) {
    assert.throws(SyntaxError, () => new RegExp(term + quantifier));
  }
}
