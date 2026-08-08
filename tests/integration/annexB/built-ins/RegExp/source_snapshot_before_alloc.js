/*---
description: >
  Constructing a RegExp from an existing RegExp reads its source and flags before allocating the
  new RegExp, which may run user code when the new target is a proxy.
---*/

// A new target whose "prototype" lookup recompiles the source regexp. RegExpAlloc reads
// newTarget.prototype, so the trap runs after the source and flags have been snapshotted but
// before they are used.
function recompilingNewTarget(regexp, source, flags) {
  return new Proxy(RegExp, {
    get: function (target, key, receiver) {
      if (key === "prototype") {
        regexp.compile(source, flags);
      }
      return Reflect.get(target, key, receiver);
    },
  });
}

// With a flags argument the copy takes the snapshotted source and the given flags.
function assertSnapshotWithFlags(sourceFlags, recompileSource, recompileFlags, newFlags) {
  var original = new RegExp("original", sourceFlags);
  var newTarget = recompilingNewTarget(original, recompileSource, recompileFlags);
  var copy = Reflect.construct(RegExp, [original, newFlags], newTarget);
  var message =
    "/original/" + sourceFlags +
    " recompiled to /" + recompileSource + "/" + recompileFlags +
    " while allocating a copy with flags " + JSON.stringify(newFlags);

  assert.sameValue(copy.source, "original", message + " keeps the snapshotted source");
  assert.sameValue(copy.flags, new RegExp("original", newFlags).flags, message + " flags");
  assert.sameValue(copy.test("original"), true, message + " matches the snapshotted source");
  assert.sameValue(copy.test(recompileSource), false, message + " does not match the new source");

  // The source regexp itself is left recompiled.
  assert.sameValue(original.source, recompileSource, message + " recompiles the source regexp");
}

// The recompiled flags equal the requested flags, so the recompiled regexp would otherwise be
// reused directly.
assertSnapshotWithFlags("g", "recompiled", "g", "g");
assertSnapshotWithFlags("", "recompiled", "", "");
assertSnapshotWithFlags("i", "recompiled", "i", "i");

// The recompiled flags differ from the requested flags only in flags that do not affect
// compilation, so the recompiled bytecode would otherwise be copied.
assertSnapshotWithFlags("", "recompiled", "y", "y");
assertSnapshotWithFlags("g", "recompiled", "dgy", "dgy");

// The recompiled flags differ in a flag that affects compilation, so the pattern is reparsed
// either way.
assertSnapshotWithFlags("", "recompiled", "u", "i");
assertSnapshotWithFlags("g", "recompiled", "i", "gs");

// Without a flags argument both the source and the flags come from the snapshot.
function assertSnapshotWithoutFlags(sourceFlags, recompileSource, recompileFlags) {
  var original = new RegExp("original", sourceFlags);
  var newTarget = recompilingNewTarget(original, recompileSource, recompileFlags);
  var copy = Reflect.construct(RegExp, [original], newTarget);
  var message =
    "/original/" + sourceFlags +
    " recompiled to /" + recompileSource + "/" + recompileFlags +
    " while allocating a copy with no flags argument";

  assert.sameValue(copy.source, "original", message + " keeps the snapshotted source");
  assert.sameValue(copy.flags, new RegExp("original", sourceFlags).flags, message + " flags");
  assert.sameValue(copy.test("original"), true, message + " matches the snapshotted source");
  assert.sameValue(copy.test(recompileSource), false, message + " does not match the new source");

  assert.sameValue(original.source, recompileSource, message + " recompiles the source regexp");
}

assertSnapshotWithoutFlags("g", "recompiled", "g");
assertSnapshotWithoutFlags("", "recompiled", "i");
assertSnapshotWithoutFlags("i", "recompiled", "");
assertSnapshotWithoutFlags("dgy", "recompiled", "u");

// Recompiling the source regexp to a pattern that would be invalid under the requested flags must
// not make the construction fail, since the snapshotted pattern is what gets compiled.
var bracketOriginal = new RegExp("original", "");
var bracketCopy = Reflect.construct(
  RegExp,
  [bracketOriginal, "v"],
  recompilingNewTarget(bracketOriginal, "[[]", "")
);
assert.sameValue(bracketCopy.source, "original");
assert.sameValue(bracketCopy.flags, "v");
assert.sameValue(bracketCopy.test("original"), true);

// Conversely, a source that is invalid under the requested flags still throws even though the
// source regexp was recompiled to something valid.
var invalidOriginal = new RegExp("[[]", "");
assert.throws(SyntaxError, function () {
  Reflect.construct(
    RegExp,
    [invalidOriginal, "v"],
    recompilingNewTarget(invalidOriginal, "original", "")
  );
});

// The snapshot is taken before the prototype lookup, so the copy still gets the prototype the trap
// returns. RegExp itself cannot be used as the proxy target here since its `prototype` property is
// non-writable and non-configurable, so a trap may not report a different value for it.
var protoOriginal = new RegExp("original", "g");
var customProto = {};
function ProtoNewTargetTarget() {}
var protoNewTarget = new Proxy(ProtoNewTargetTarget, {
  get: function (target, key, receiver) {
    if (key === "prototype") {
      protoOriginal.compile("recompiled", "g");
      return customProto;
    }
    return Reflect.get(target, key, receiver);
  },
});
var protoCopy = Reflect.construct(RegExp, [protoOriginal, "g"], protoNewTarget);
assert.sameValue(Object.getPrototypeOf(protoCopy), customProto);
assert.sameValue(
  Object.getOwnPropertyDescriptor(protoCopy, "lastIndex").value,
  0,
  "the copy is still initialized"
);
// The `source` getter and `exec` read internal slots, unlike the `flags` getter which collects
// ordinary property lookups that the custom prototype does not provide.
var sourceGetter = Object.getOwnPropertyDescriptor(RegExp.prototype, "source").get;
assert.sameValue(sourceGetter.call(protoCopy), "original");
assert.sameValue(RegExp.prototype.exec.call(protoCopy, "xoriginaly")[0], "original");
assert.sameValue(RegExp.prototype.exec.call(protoCopy, "xrecompiledy"), null);
