/*---
description: >
  Construct cache entries are invalidated when new.target's `prototype` value changes or
  new.target's shape changes, and the site refills with the new state.
---*/

// Reassigning `prototype` to another object is observed by a warm site, including switching back
// to a previously cached prototype.
(function () {
  function F() {}
  var original = F.prototype;
  var other = { other: true };

  function make() { return new F(); }
  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(make()), original);
  }

  F.prototype = other;
  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(make()), other);
    assert.sameValue(make().other, true);
  }

  F.prototype = original;
  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(make()), original);
    assert.sameValue(make().other, undefined);
  }
})();

// A non-object `prototype` falls back to %Object.prototype%, and the site caches again once
// `prototype` is an object.
(function () {
  function F() { this.x = 1; }
  function make() { return new F(); }
  make();
  make();

  var primitives = [42, "str", null, undefined, true, Symbol("sym")];
  for (var i = 0; i < primitives.length; i++) {
    F.prototype = primitives[i];
    assert.sameValue(Object.getPrototypeOf(make()), Object.prototype);
    assert.sameValue(Object.getPrototypeOf(make()), Object.prototype);
    assert.sameValue(make().x, 1);
  }

  var proto = { fromProto: 1 };
  F.prototype = proto;
  for (var i = 0; i < 3; i++) {
    assert.sameValue(Object.getPrototypeOf(make()), proto);
  }
})();

// Mutating the prototype object itself does not change which prototype receivers get, and
// receivers observe the mutation.
(function () {
  function F() {}
  function make() { return new F(); }
  var a = make();
  var b = make();

  F.prototype.added = "yes";
  var c = make();
  assert.sameValue(Object.getPrototypeOf(c), F.prototype);
  assert.sameValue(a.added, "yes");
  assert.sameValue(c.added, "yes");

  delete F.prototype.added;
  assert.sameValue(make().added, undefined);
})();

// Changing new.target's shape (adding or deleting own properties, changing its prototype,
// preventing extensions, freezing) refills the cache and stays correct.
(function () {
  function F() { this.x = 1; }
  var proto = F.prototype;
  function check() {
    for (var i = 0; i < 3; i++) {
      var o = new F();
      assert.sameValue(Object.getPrototypeOf(o), proto);
      assert.sameValue(o.x, 1);
    }
  }

  check();

  F.staticProp = 1;
  check();

  delete F.staticProp;
  check();

  Object.setPrototypeOf(F, { __proto__: Function.prototype, inherited: true });
  check();
  assert.sameValue(F.inherited, true);

  Object.preventExtensions(F);
  check();

  Object.freeze(F);
  check();
})();

// Reassigning `prototype` together with a shape change.
(function () {
  function F() {}
  function make() { return new F(); }
  make();
  make();

  var proto = {};
  F.extra = 1;
  F.prototype = proto;
  assert.sameValue(Object.getPrototypeOf(make()), proto);
  assert.sameValue(Object.getPrototypeOf(make()), proto);
})();

// `prototype` is read after the arguments are evaluated, so a reassignment during argument
// evaluation is observed by that construction.
(function () {
  function F(value) { this.value = value; }
  function make(arg) { return new F(arg); }
  make(0);
  make(0);

  var proto = { reassigned: true };
  var o = make((F.prototype = proto, 1));
  assert.sameValue(Object.getPrototypeOf(o), proto);
  assert.sameValue(o.value, 1);
})();

// Reassigning `prototype` inside the constructor only affects later constructions, since the
// receiver was already created.
(function () {
  var protos = [{ id: 0 }, { id: 1 }, { id: 2 }, { id: 3 }];
  var count = 0;
  function F() {
    count++;
    F.prototype = protos[count];
  }
  F.prototype = protos[0];

  for (var i = 0; i < 3; i++) {
    var o = new F();
    assert.sameValue(Object.getPrototypeOf(o), protos[i]);
  }
})();

// A class `prototype` is not writable, so the cache stays valid when an assignment is attempted.
(function () {
  class C {
    constructor() { this.c = true; }
  }
  var proto = C.prototype;
  function make() { return new C(); }
  make();
  make();

  assert.throws(TypeError, function () {
    "use strict";
    C.prototype = {};
  });
  assert.sameValue(C.prototype, proto);
  assert.sameValue(Object.getPrototypeOf(make()), proto);
  assert.sameValue(make().c, true);
})();

// A map mode new.target that is also a prototype object keeps its shape when its properties map is
// reallocated. Reassigning `prototype` after the map grows must still be observed, even when the
// previously cached location now holds the old prototype.
(function () {
  function F() {}
  var oldProto = F.prototype;
  var newProto = { isNew: true };

  // Enter map mode, filling every other property with the old prototype
  F.a = 1;
  delete F.a;
  Object.defineProperty(F, "length", { value: oldProto });
  Object.defineProperty(F, "name", { value: oldProto });
  F.q = oldProto;

  // Cached lookup through F makes F a prototype object
  var child = Object.create(F);
  function get(o) { return o.q; }
  get(child);
  get(child);

  function make() { return new F(); }
  make();
  make();

  // Grow the map without changing F's shape
  for (var i = 0; i < 10; i++) {
    F["s" + i] = oldProto;
  }

  F.prototype = newProto;
  for (var i = 0; i < 3; i++) {
    var o = make();
    assert.sameValue(Object.getPrototypeOf(o), newProto);
    assert.sameValue(o instanceof F, true);
    assert.sameValue(o.isNew, true);
  }
})();

// Same as above, but the map is compacted at the same capacity which moves `prototype` to an earlier
// entry.
(function () {
  function F() {}
  var oldProto = F.prototype;
  var newProto = { isNew: true };

  F.a = 1;
  delete F.a;
  F.q = oldProto;

  var child = Object.create(F);
  function get(o) { return o.q; }
  get(child);
  get(child);

  function make() { return new F(); }
  make();
  make();

  // Fill the map, then delete enough entries that the next insertion compacts it in place
  F.s0 = oldProto;
  F.s1 = oldProto;
  F.s2 = oldProto;
  delete F.length;
  delete F.name;
  delete F.s0;
  delete F.s1;
  F.x = oldProto;

  F.prototype = newProto;
  for (var i = 0; i < 3; i++) {
    var o = make();
    assert.sameValue(Object.getPrototypeOf(o), newProto);
    assert.sameValue(o instanceof F, true);
  }
})();
