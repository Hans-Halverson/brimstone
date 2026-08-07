/*---
description: Cached substitution templates are aware of named capture availability per match.
---*/

function regexpWithMatches(matches) {
  var regexp = new RegExp("b", "g");
  var index = 0;

  regexp.exec = function () {
    return index < matches.length ? matches[index++] : null;
  };

  return regexp;
}

function matchAt(index, groups) {
  return { 0: "b", index: index, groups: groups };
}

// Named captures only allowed on the second match
assert.sameValue(
  "abcb".replace(regexpWithMatches([matchAt(1), matchAt(3, { name: "Q" })]), "<$<name>>"),
  "a<$<name>>c<Q>"
);

// Named captures are only allowed on the first match
assert.sameValue(
  "abcb".replace(regexpWithMatches([matchAt(1, { name: "Q" }), matchAt(3)]), "<$<name>>"),
  "a<Q>c<$<name>>"
);

// Alternating availability of named captures multiple times across matches
assert.sameValue(
  "abcbeb".replace(
    regexpWithMatches([matchAt(1, { name: "1" }), matchAt(3), matchAt(5, { name: "3" })]),
    "<$<name>>"
  ),
  "a<1>c<$<name>>e<3>",
);

// Every match has named captures
assert.sameValue(
  "abcb".replace(
    regexpWithMatches([matchAt(1, { name: "X" }), matchAt(3, { name: "Y" })]),
    "<$<name>>"
  ),
  "a<X>c<Y>",
);
