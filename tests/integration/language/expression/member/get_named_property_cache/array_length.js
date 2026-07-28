/*---
description: Array length reads are cached, only checking receiver kind.
---*/

// Each site is read in a loop so the inline cache is filled and then re-matched.
function readLength(o) {
  var last;
  for (var i = 0; i < 100; i++) {
    last = o.length;
  }
  return last;
}

// Length tracks mutations after the cache is filled
var mutated = [1, 2, 3];
assert.sameValue(readLength(mutated), 3);
mutated.push(4);
assert.sameValue(readLength(mutated), 4);
mutated.pop();
mutated.pop();
assert.sameValue(readLength(mutated), 2);
mutated.length = 5;
assert.sameValue(readLength(mutated), 5);
mutated.length = 0;
assert.sameValue(readLength(mutated), 0);

// Sparse arrays and element deletes do not shrink length
var sparse = [1, 2, 3, 4];
delete sparse[1];
assert.sameValue(readLength(sparse), 4);
sparse[100] = 1;
assert.sameValue(readLength(sparse), 101);

// A single cache entry must serve every array shape, not just the first one seen
var shapes = [];
for (var i = 0; i < 10; i++) {
  var arr = [];
  for (var j = 0; j <= i; j++) {
    arr.push(j);
  }
  // Give each array a distinct shape by adding different named properties
  for (var j = 0; j < i; j++) {
    arr["p" + j] = j;
  }
  shapes.push(arr);
}
for (var i = 0; i < 100; i++) {
  for (var j = 0; j < shapes.length; j++) {
    assert.sameValue(shapes[j].length, j + 1);
  }
}

// Arrays in map (dictionary) mode still report length correctly
var mapMode = [1, 2, 3];
mapMode.extra = 1;
delete mapMode.extra;
assert.sameValue(readLength(mapMode), 3);
mapMode.push(4);
assert.sameValue(readLength(mapMode), 4);

// Non-writable length blocks stores but reads stay correct
var frozen = [1, 2, 3];
Object.freeze(frozen);
assert.sameValue(readLength(frozen), 3);
assert.sameValue(Object.getOwnPropertyDescriptor(frozen, "length").writable, false);

// length is non-configurable, so it can never become an accessor or be deleted
var fixed = [1, 2, 3];
assert.sameValue(readLength(fixed), 3);
assert.throws(TypeError, function () {
  Object.defineProperty(fixed, "length", { get: function () { return 99; } });
});
assert.sameValue(readLength(fixed), 3);
assert.throws(TypeError, function () {
  "use strict";
  delete fixed.length;
});
assert.sameValue(readLength(fixed), 3);

// Array subclasses are array exotic objects too
class Subclass extends Array {}
var sub = new Subclass();
sub.push(1);
sub.push(2);
assert.sameValue(readLength(sub), 2);

// Array.prototype is itself an array exotic object with length 0
assert.sameValue(readLength(Array.prototype), 0);

// Prototype mutation does not affect the own synthesized length
var reproto = [1, 2, 3, 4];
assert.sameValue(readLength(reproto), 4);
Object.setPrototypeOf(reproto, null);
assert.sameValue(readLength(reproto), 4);

// Lengths above the small-integer range are reported as numbers, not truncated
var huge = new Array(3000000000);
assert.sameValue(readLength(huge), 3000000000);
assert.sameValue(typeof readLength(huge), "number");
assert.sameValue(readLength(new Array(4294967295)), 4294967295);

// A site that alternates between arrays and non-arrays must stay correct for both
function polymorphic(o) {
  return o.length;
}
var plain = { length: 7 };
var str = new String("abcd");
for (var i = 0; i < 100; i++) {
  assert.sameValue(polymorphic(mutated), 0);
  assert.sameValue(polymorphic(plain), 7);
  assert.sameValue(polymorphic(str), 4);
  assert.sameValue(polymorphic(shapes[3]), 4);
}

// An ordinary object inheriting from Array.prototype is not an array
(function () {
  function len(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.length;
    }
    return last;
  }

  assert.sameValue(len([1, 2, 3]), 3);

  var inherited = Object.create(Array.prototype);
  assert.sameValue(len(inherited), 0);
  inherited.length = 3;
  assert.sameValue(len(inherited), 3);
})();

// A proxy wrapping an array must still route through its trap
(function () {
  function len(o) {
    var last;
    for (var i = 0; i < 100; i++) {
      last = o.length;
    }
    return last;
  }

  assert.sameValue(len([1, 2, 3]), 3);

  var trapped = 0;
  var proxy = new Proxy([1, 2, 3], {
    get: function (target, key, receiver) {
      if (key === "length") {
        trapped++;
      }
      return Reflect.get(target, key, receiver);
    },
  });
  assert.sameValue(len(proxy), 3);
  assert.sameValue(trapped, 100);
})();
