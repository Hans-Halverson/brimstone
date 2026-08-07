/*---
description: >
  Constructing a RegExp from an existing RegExp plus a flags argument uses values from the source
  RegExp before converting the flags to a string, which may run user code.
---*/

function assertSnapshot(sourceFlags, recompileSource, recompileFlags, newFlags) {
  var original = new RegExp("original", sourceFlags);
  var flagsArgument = {
    toString: function () {
      original.compile(recompileSource, recompileFlags);
      return newFlags;
    },
  };

  var copy = new RegExp(original, flagsArgument);
  var message =
    "/original/" + sourceFlags +
    " recompiled to /" + recompileSource + "/" + recompileFlags +
    " while converting flags to " + JSON.stringify(newFlags);

  assert.sameValue(copy.source, "original", message + " keeps the snapshotted source");
  assert.sameValue(copy.flags, new RegExp("original", newFlags).flags, message + " flags");
  assert.sameValue(copy.test("original"), true, message + " matches the snapshotted source");
  assert.sameValue(copy.test(recompileSource), false, message + " does not match the new source");

  // The source regexp itself is left recompiled.
  assert.sameValue(original.source, recompileSource, message + " recompiles the source regexp");
}

// The recompiled flags equal the requested flags, so the recompiled regexp would otherwise be
// reused directly.
assertSnapshot("g", "recompiled", "g", "g");
assertSnapshot("", "recompiled", "", "");
assertSnapshot("i", "recompiled", "i", "i");

// The recompiled flags differ from the requested flags only in flags that do not affect
// compilation, so the recompiled bytecode would otherwise be copied.
assertSnapshot("", "recompiled", "y", "y");
assertSnapshot("g", "recompiled", "dgy", "dgy");
assertSnapshot("i", "recompiled", "gi", "i");

// The recompiled flags differ in a flag that affects compilation, so the pattern is reparsed
// either way.
assertSnapshot("", "recompiled", "u", "i");
assertSnapshot("g", "recompiled", "i", "gs");

// Recompiling the source regexp to a pattern that would be invalid under the requested flags must
// not make the construction fail, since the snapshotted pattern is what gets compiled.
var bracketOriginal = new RegExp("original", "");
var bracketFlags = {
  toString: function () {
    bracketOriginal.compile("[[]", "");
    return "v";
  },
};
var bracketCopy = new RegExp(bracketOriginal, bracketFlags);
assert.sameValue(bracketCopy.source, "original");
assert.sameValue(bracketCopy.flags, "v");
assert.sameValue(bracketCopy.test("original"), true);

// Conversely, a source that is invalid under the requested flags still throws even though the
// source regexp was recompiled to something valid.
var invalidOriginal = new RegExp("[[]", "");
var invalidFlags = {
  toString: function () {
    invalidOriginal.compile("original", "");
    return "v";
  },
};
assert.throws(SyntaxError, function () {
  new RegExp(invalidOriginal, invalidFlags);
});

// RegExp.prototype.compile itself reuses the compiled regexp when given another RegExp, and rejects
// a flags argument in that case.
var compiled = new RegExp("abc", "g");
compiled.compile(new RegExp("(x)y", "i"));
assert.sameValue(compiled.source, "(x)y");
assert.sameValue(compiled.flags, "i");
assert.sameValue(compiled.lastIndex, 0);
assert.sameValue(compiled.test("XY"), true);

assert.throws(TypeError, function () {
  new RegExp("a", "").compile(new RegExp("b", ""), "g");
});
