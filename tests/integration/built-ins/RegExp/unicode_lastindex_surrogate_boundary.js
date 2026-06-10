/*---
description: ^
  In unicode mode if `lastIndex` points to the middle of a valid surrogate pair then snap it to the
  beginning of the encoded code point.
---*/

// Code units are "\uD83D\uDE00"
const str = "a\u{1F600}b";

// RegExp.prototype.exec
{
  const regexp = /./gu;
  regexp.lastIndex = 2;
  const result = regexp.exec(str);
  assert.compareArray(result, ["\u{1F600}"]);
  assert.sameValue(result.index, 1);
  assert.sameValue(regexp.lastIndex, 3);
}

// RegExp.prototype.test
{
  const regexp = /\u{1F600}/yu;
  regexp.lastIndex = 2;
  assert.sameValue(regexp.test(str), true);
  assert.sameValue(regexp.lastIndex, 3);
}

// RegExp.prototype.match
{
  const regexp = /\u{1F600}/yu;
  regexp.lastIndex = 2;
  const result = str.match(regexp);
  assert.compareArray(result, ["\u{1F600}"]);
  assert.sameValue(result.index, 1);
}

// RegExp.prototype.matchAll
{
  const regexp = /./gu;
  regexp.lastIndex = 2;
  const ms = [...str.matchAll(regexp)];
  assert.sameValue(ms.length, 2);
  assert.compareArray(ms[0], ["\u{1F600}"]);
  assert.sameValue(ms[0].index, 1);
  assert.compareArray(ms[1], ["b"]);
  assert.sameValue(ms[1].index, 3);
}

// RegExp.prototype.replace
{
  const regexp = /\u{1F600}/yu;
  regexp.lastIndex = 2;
  assert.sameValue(str.replace(regexp, "X"), "aXb");
}

// Do not snap for unpaired high surrogate
{
  const regexp = /./gu;
  regexp.lastIndex = 1;
  const result = regexp.exec("a\uD83Db");
  assert.compareArray(result, ["\uD83D"]);
  assert.sameValue(result.index, 1);
  assert.sameValue(regexp.lastIndex, 2);
}

// Do not snap for unpaired low surrogate
{
  const regexp = /./gu;
  regexp.lastIndex = 1;
  const result = regexp.exec("a\uDE00b");
  assert.compareArray(result, ["\uDE00"]);
  assert.sameValue(result.index, 1);
  assert.sameValue(regexp.lastIndex, 2);
}

// Do not snap for pair of low surrogates
{
  const regexp = /./yu;
  regexp.lastIndex = 2;
  const result = regexp.exec("x\uDE00\uDE00");
  assert.compareArray(result, ["\uDE00"]);
  assert.sameValue(result.index, 2);
}

// Do not snap for pair of high surrogates
{
  const regexp = /./yu;
  regexp.lastIndex = 2;
  const result = regexp.exec("x\uD83D\uD83D");
  assert.compareArray(result, ["\uD83D"]);
  assert.sameValue(result.index, 2);
}
