/*---
description: Array indexed properties with non-default attributes.
---*/

// Non-default attributes survive filling a sparse array densely.
(function () {
  var a = [];
  a[2000] = "far";
  Object.defineProperty(a, 100, {
    value: "fixed",
    writable: false,
    enumerable: false,
    configurable: false,
  });

  for (var i = 0; i < 2000; i++) {
    if (i !== 100) {
      a[i] = i;
    }
  }

  assert.sameValue(a[100], "fixed");
  assert.sameValue(a[1999], 1999);
  assert.sameValue(a.length, 2001);

  var desc = Object.getOwnPropertyDescriptor(a, 100);
  assert.sameValue(desc.writable, false);
  assert.sameValue(desc.enumerable, false);
  assert.sameValue(desc.configurable, false);

  // Non-enumerable indexed property is skipped by enumeration but still readable.
  assert.sameValue(Object.keys(a).indexOf("100"), -1);
  assert.sameValue(100 in a, true);

  // Writes to the non-writable indexed property fail.
  assert.throws(TypeError, function () {
    "use strict";
    a[100] = "changed";
  });
  assert.sameValue(a[100], "fixed");
})();

// Accessors on array indexes survive dense fills and keep being invoked.
(function () {
  var a = [];
  a[1500] = "far";
  var gets = 0;
  Object.defineProperty(a, 50, {
    get: function () { gets++; return "got"; },
    configurable: true,
  });

  for (var i = 0; i < 1500; i++) {
    if (i !== 50) {
      a[i] = i;
    }
  }

  assert.sameValue(a[50], "got");
  assert.sameValue(a[50], "got");
  assert.sameValue(gets, 2);
  assert.sameValue(a[1499], 1499);
})();

// Defining a non-default indexed property preserves the other indexed properties.
(function () {
  var a = [];
  for (var i = 0; i < 1200; i++) {
    a[i] = i;
  }
  Object.defineProperty(a, 600, { value: "mid", writable: false });

  assert.sameValue(a[600], "mid");
  assert.sameValue(a[599], 599);
  assert.sameValue(a[601], 601);
  assert.sameValue(a.length, 1200);

  // The array continues to accept normal stores and grows.
  a[1200] = "end";
  assert.sameValue(a[1200], "end");
  assert.sameValue(a.length, 1201);
})();

// Length shrink stops at a non-configurable indexed property.
(function () {
  var a = [];
  a[2000] = "far";
  Object.defineProperty(a, 5, { value: "keep", configurable: false });

  try {
    a.length = 3;
  } catch (e) {
    // Throws in strict mode after partially shrinking
  }

  assert.sameValue(a.length, 6);
  assert.sameValue(a[5], "keep");
  assert.sameValue(a[2000], undefined);
})();

// Frozen arrays reject all indexed property stores and length changes.
(function () {
  var a = [1, 2, 3];
  Object.freeze(a);
  assert.throws(TypeError, function () {
    "use strict";
    a[0] = 10;
  });
  assert.throws(TypeError, function () {
    "use strict";
    a[3] = 4;
  });
  assert.throws(TypeError, function () {
    "use strict";
    a.length = 1;
  });
  assert.sameValue(a.join(","), "1,2,3");
})();

// Non-default attributes on a dense array are preserved instead of stored densely.
(function () {
  var a = [1, 2, 3];
  Object.defineProperty(a, 1, {
    value: "fixed",
    writable: false,
    enumerable: false,
    configurable: false,
  });

  var desc = Object.getOwnPropertyDescriptor(a, 1);
  assert.sameValue(desc.value, "fixed");
  assert.sameValue(desc.writable, false);
  assert.sameValue(desc.enumerable, false);
  assert.sameValue(desc.configurable, false);

  assert.sameValue(Object.keys(a).indexOf("1"), -1);
  assert.sameValue(1 in a, true);
  assert.throws(TypeError, function () {
    "use strict";
    a[1] = "changed";
  });
  assert.sameValue(a[1], "fixed");

  // Surrounding indexed properties are untouched.
  assert.sameValue(a[0], 1);
  assert.sameValue(a[2], 3);
  assert.sameValue(a.length, 3);
})();

// Accessors defined on a dense array keep accessor semantics.
(function () {
  var a = [1, 2, 3];
  var gets = 0;
  var sets = 0;
  Object.defineProperty(a, 0, {
    get: function () {
      gets++;
      return "got";
    },
    set: function () {
      sets++;
    },
    configurable: true,
  });

  assert.sameValue(a[0], "got");
  assert.sameValue(gets, 1);

  a[0] = "ignored";
  assert.sameValue(sets, 1);
  assert.sameValue(a[0], "got");

  var desc = Object.getOwnPropertyDescriptor(a, 0);
  assert.sameValue(typeof desc.get, "function");
  assert.sameValue(typeof desc.set, "function");
  assert.sameValue(desc.value, undefined);
  assert.sameValue(a[1], 2);
})();

// Non-default attributes survive shrinking a sparse array and then refilling it densely.
(function () {
  var a = [];
  a[5000] = "far";
  Object.defineProperty(a, 10, {
    value: "fixed",
    writable: false,
    enumerable: false,
    configurable: false,
  });

  // Shrinking rebuilds the sparse map, which must not forget the non-default property.
  a.length = 200;
  assert.sameValue(a.length, 200);
  assert.sameValue(a[10], "fixed");
  assert.sameValue(5000 in a, false);

  // Refill densely. The array must not be stored densely again, which would drop attributes.
  for (var i = 0; i < 200; i++) {
    if (i !== 10) {
      a[i] = i;
    }
  }

  var desc = Object.getOwnPropertyDescriptor(a, 10);
  assert.sameValue(desc.value, "fixed");
  assert.sameValue(desc.writable, false);
  assert.sameValue(desc.enumerable, false);
  assert.sameValue(desc.configurable, false);

  assert.sameValue(Object.keys(a).indexOf("10"), -1);
  assert.sameValue(a[0], 0);
  assert.sameValue(a[199], 199);
  assert.throws(TypeError, function () {
    "use strict";
    a[10] = "changed";
  });
})();

// An accessor survives shrinking a sparse array and then refilling it densely.
(function () {
  var a = [];
  a[4000] = "far";
  var gets = 0;
  Object.defineProperty(a, 20, {
    get: function () {
      gets++;
      return "got";
    },
    configurable: true,
  });

  a.length = 300;
  assert.sameValue(a.length, 300);
  assert.sameValue(a[20], "got");
  assert.sameValue(gets, 1);

  for (var i = 0; i < 300; i++) {
    if (i !== 20) {
      a[i] = i;
    }
  }

  assert.sameValue(a[20], "got");
  assert.sameValue(gets, 2);
  assert.sameValue(a[299], 299);
})();
