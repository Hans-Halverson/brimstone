/*---
description: The private `in` operator is PrivateElementFind, not [[HasProperty]].
---*/

// A private field is found only on the object that it was installed on.
(function () {
  class C {
    #f = 1;
    static has(o) { return #f in o; }
  }

  assert.sameValue(C.has(new C()), true);
  assert.sameValue(C.has({}), false);
})();

// Private methods and accessors are brands installed on the instance, and are found the same way.
(function () {
  class C {
    #m() {}
    get #a() { return 1; }
    static hasMethod(o) { return #m in o; }
    static hasAccessor(o) { return #a in o; }
  }

  var instance = new C();
  assert.sameValue(C.hasMethod(instance), true);
  assert.sameValue(C.hasAccessor(instance), true);
  assert.sameValue(C.hasMethod({}), false);
  assert.sameValue(C.hasAccessor({}), false);
})();

// Static private elements are installed on the constructor, not on instances.
(function () {
  class C {
    static #f = 1;
    static has(o) { return #f in o; }
  }

  assert.sameValue(C.has(C), true);
  assert.sameValue(C.has(new C()), false);
})();

// There is no prototype walk: a private element on the prototype is not visible on the object.
(function () {
  class C {
    #f = 1;
    static has(o) { return #f in o; }
  }

  var proto = new C();
  var derived = Object.create(proto);

  assert.sameValue(C.has(proto), true);
  assert.sameValue(C.has(derived), false);
})();

// Ordinary properties are still found through the prototype chain, so the private path must not
// leak into the generic `in` operator.
(function () {
  var derived = Object.create({ x: 1 });
  assert.sameValue('x' in derived, true);
})();

// No proxy trap is called, and the target's private elements are not seen through the proxy.
(function () {
  class C {
    #f = 1;
    static has(o) { return #f in o; }
  }

  var trapCalls = 0;
  var proxy = new Proxy(new C(), {
    has: function () {
      trapCalls++;
      return true;
    },
    getOwnPropertyDescriptor: function () {
      trapCalls++;
      return undefined;
    },
  });

  assert.sameValue(C.has(proxy), false);
  assert.sameValue(trapCalls, 0);

  // The same proxy still runs its `has` trap for a normal `in`.
  assert.sameValue('x' in proxy, true);
  assert.sameValue(trapCalls, 1);
})();

// A revoked proxy would throw from [[HasProperty]], but private `in` never reaches it.
(function () {
  class C {
    #f = 1;
    static has(o) { return #f in o; }
  }

  var revocable = Proxy.revocable(new C(), {});
  revocable.revoke();

  assert.sameValue(C.has(revocable.proxy), false);
  assert.throws(TypeError, function () { 'x' in revocable.proxy; });
})();

// The right side must still be an object.
(function () {
  class C {
    #f = 1;
    static has(o) { return #f in o; }
  }

  assert.throws(TypeError, function () { C.has(1); });
  assert.throws(TypeError, function () { C.has(null); });
  assert.throws(TypeError, function () { C.has(undefined); });
  assert.throws(TypeError, function () { C.has('string'); });
})();

// Private elements of one class are distinct from the identically named elements of another.
(function () {
  class C {
    #f = 1;
    static has(o) { return #f in o; }
  }

  class D {
    #f = 2;
  }

  assert.sameValue(C.has(new D()), false);
})();
