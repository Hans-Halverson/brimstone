/*---
description: Array indexed properties in both dense and sparse storage.
---*/

// Array constructor presizes without creating own indexed properties.
(function () {
  var a = new Array(2000);
  assert.sameValue(a.length, 2000);
  assert.sameValue(0 in a, false);
  assert.sameValue(1999 in a, false);
  assert.sameValue(a[0], undefined);
  assert.sameValue(Object.keys(a).length, 0);
})();

// Presized array can have all indexed properties filled in and read back.
(function () {
  var a = new Array(2000);
  for (var i = 0; i < 2000; i++) {
    a[i] = i;
  }
  var sum = 0;
  for (var i = 0; i < 2000; i++) {
    sum += a[i];
  }
  assert.sameValue(sum, 2000 * 1999 / 2);
  assert.sameValue(a.length, 2000);
  assert.sameValue(Object.keys(a).length, 2000);
})();

// Growing the length presizes, preserving existing indexed properties.
(function () {
  var a = [1, 2, 3];
  a.length = 3000;
  assert.sameValue(a.length, 3000);
  assert.sameValue(a[0], 1);
  assert.sameValue(a[2], 3);
  assert.sameValue(a[2999], undefined);
  assert.sameValue(2999 in a, false);
  assert.sameValue(Object.keys(a).length, 3);
})();

// Appending just past a presized array extends it.
(function () {
  var a = new Array(1500);
  a.push("end");
  assert.sameValue(a.length, 1501);
  assert.sameValue(a[1500], "end");
  a[1501] = "next";
  assert.sameValue(a.length, 1502);
})();

// Shrinking removes indexed properties; re-growing does not resurrect them.
(function () {
  var a = new Array(2000);
  a[1500] = "x";
  a.length = 100;
  assert.sameValue(a.length, 100);
  assert.sameValue(a[1500], undefined);
  a.length = 2000;
  assert.sameValue(a[1500], undefined);
  assert.sameValue(1500 in a, false);
})();

// Enumeration only visits indexed properties that exist, in ascending order.
(function () {
  var a = new Array(2000);
  a[7] = "a";
  a[3] = "b";
  a[1999] = "c";
  var keys = [];
  for (var key in a) {
    keys.push(key);
  }
  assert.sameValue(keys.join(","), "3,7,1999");
  assert.sameValue(Object.keys(a).join(","), "3,7,1999");
})();

// Common methods treat holes as undefined but skip them where specified.
(function () {
  var a = new Array(1200);
  assert.sameValue(a.indexOf(undefined), -1);
  assert.sameValue(a.fill(2).reduce(function (x, y) { return x + y; }, 0), 2400);

  var b = new Array(1100);
  b[0] = 1;
  var visited = 0;
  b.forEach(function () { visited++; });
  assert.sameValue(visited, 1);
})();

// A store far past the end of an empty array is sparse but fully functional.
(function () {
  var a = [];
  a[5000] = "far";
  assert.sameValue(a.length, 5001);
  assert.sameValue(a[5000], "far");
  assert.sameValue(a[0], undefined);
  assert.sameValue(4999 in a, false);
  assert.sameValue(Object.keys(a).join(","), "5000");

  delete a[5000];
  assert.sameValue(5000 in a, false);
  assert.sameValue(a.length, 5001);
})();

// Filling a sparse array from the front converts it back to dense storage.
(function () {
  var a = [];
  a[5000] = "far";
  for (var i = 0; i < 5000; i++) {
    a[i] = i;
  }
  assert.sameValue(a.length, 5001);
  assert.sameValue(a[5000], "far");
  var sum = 0;
  for (var i = 0; i < 5000; i++) {
    sum += a[i];
  }
  assert.sameValue(sum, 5000 * 4999 / 2);
  assert.sameValue(Object.keys(a).length, 5001);
})();

// Filling back-to-front also densifies, preserving ascending enumeration order.
(function () {
  var a = [];
  for (var i = 3000; i >= 0; i--) {
    a[i] = i;
  }
  assert.sameValue(a.length, 3001);
  assert.sameValue(a[0], 0);
  assert.sameValue(a[3000], 3000);

  var keys = Object.keys(a);
  assert.sameValue(keys.length, 3001);
  assert.sameValue(keys[0], "0");
  assert.sameValue(keys[3000], "3000");
})();

// Holes remaining after densification still read from the prototype chain.
(function () {
  var a = [];
  a[4000] = "far";
  for (var i = 0; i < 1500; i++) {
    a[i] = i;
  }
  Array.prototype[2000] = "proto";
  try {
    assert.sameValue(a[2000], "proto");
    assert.sameValue(a.hasOwnProperty(2000), false);
    assert.sameValue(a[1499], 1499);
  } finally {
    delete Array.prototype[2000];
  }
})();

// Repeated grows and shrinks across representations keep values consistent.
(function () {
  var a = [];
  a[2000] = "s";     // sparse
  a.length = 10;     // shrink removes it
  assert.sameValue(a[2000], undefined);
  a[5] = "d";
  a[2000] = "s2";
  assert.sameValue(a[5], "d");
  assert.sameValue(a[2000], "s2");
  assert.sameValue(a.length, 2001);
})();

// Non-array objects use the same indexed property storage transitions.
(function () {
  var o = {};
  o[4000] = "far";
  for (var i = 0; i < 1200; i++) {
    o[i] = i;
  }
  assert.sameValue(o[4000], "far");
  assert.sameValue(o[1199], 1199);
  assert.sameValue(o[3999], undefined);

  var keys = Object.keys(o);
  assert.sameValue(keys.length, 1201);
  assert.sameValue(keys[0], "0");
  assert.sameValue(keys[1200], "4000");
})();

// Lengths beyond the dense storage maximum are fully functional.
(function () {
  var len = Math.pow(2, 24) + 1;
  var a = [];
  a.length = len;
  assert.sameValue(a.length, len);

  a[len - 1] = "end";
  a[0] = "start";
  assert.sameValue(a[len - 1], "end");
  assert.sameValue(a[0], "start");
  assert.sameValue(a[12345678], undefined);
  assert.sameValue(Object.keys(a).length, 2);

  a.length = 10;
  assert.sameValue(a.length, 10);
  assert.sameValue(a[0], "start");
  assert.sameValue(len - 1 in a, false);
})();

// The Array constructor accepts lengths beyond the dense storage maximum.
(function () {
  var len = Math.pow(2, 24) + 10;
  var a = new Array(len);
  assert.sameValue(a.length, len);
  assert.sameValue(0 in a, false);

  a[len - 5] = "x";
  assert.sameValue(a[len - 5], "x");
  assert.sameValue(Object.keys(a).join(","), String(len - 5));
})();

// The maximum array index updates the length; larger keys are named properties.
(function () {
  var a = [];
  a[4294967294] = "max";
  assert.sameValue(a.length, 4294967295);
  assert.sameValue(a[4294967294], "max");

  a[4294967295] = "named";
  assert.sameValue(a.length, 4294967295);
  assert.sameValue(a[4294967295], "named");
  assert.sameValue(a.hasOwnProperty("4294967295"), true);
})();

// Shrinking a sparse array removes only indexed properties beyond the new length.
(function () {
  var a = [];
  a[3000] = "low";
  a[4000] = "high";
  a.length = 3500;
  assert.sameValue(a.length, 3500);
  assert.sameValue(a[3000], "low");
  assert.sameValue(4000 in a, false);
})();

// Array literal holes past the dense to sparse transition do not become own properties.
(function () {
  var a = eval("[" + ",".repeat(2000) + "1]");
  assert.sameValue(a.length, 2001);
  assert.sameValue(a[2000], 1);
  assert.sameValue(Object.keys(a).length, 1);

  // Holes must read back as undefined instead of leaking the empty value.
  assert.sameValue(1500 in a, false);
  assert.sameValue(a[1500], undefined);
  assert.sameValue(a[1500] + 1, NaN);

  var visited = 0;
  a.forEach(function () {
    visited++;
  });
  assert.sameValue(visited, 1);
  assert.sameValue(a.filter(function () { return true; }).length, 1);
})();

// Array literal that is entirely holes past the transition has no own properties.
(function () {
  var a = eval("[" + ",".repeat(2000) + "]");
  assert.sameValue(a.length, 2000);
  assert.sameValue(Object.keys(a).length, 0);
  assert.sameValue(0 in a, false);
  assert.sameValue(1999 in a, false);
})();

// Storing a hole into a sparse array extends the length without adding a property.
(function () {
  var a = [];
  a[5000] = 1;
  assert.sameValue(Object.keys(a).length, 1);

  a.length = 0;
  a[8000] = 2;
  assert.sameValue(a.length, 8001);
  assert.sameValue(Object.keys(a).join(","), "8000");
})();

// Indices beyond the maximum dense array length are stored sparsely.
(function () {
  var a = [1, 2, 3];
  a[20000000] = "far";
  assert.sameValue(a.length, 20000001);
  assert.sameValue(a[20000000], "far");
  assert.sameValue(Object.keys(a).length, 4);
  assert.sameValue(1000000 in a, false);
  assert.sameValue(a[1000000], undefined);
})();

// An array that becomes sparse and is then refilled densely keeps all of its properties.
(function () {
  var a = [];
  for (var i = 0; i < 100; i++) {
    a[i] = i;
  }

  // Far past the end of a mostly empty array, forcing sparse storage.
  a[50000] = "far";
  assert.sameValue(a.length, 50001);
  assert.sameValue(a[0], 0);
  assert.sameValue(a[99], 99);
  assert.sameValue(a[50000], "far");
  assert.sameValue(Object.keys(a).length, 101);
  assert.sameValue(1000 in a, false);

  // Refilling makes the array dense enough to be stored densely again.
  for (var i = 100; i < 50000; i++) {
    a[i] = i;
  }
  assert.sameValue(a.length, 50001);
  assert.sameValue(a[0], 0);
  assert.sameValue(a[49999], 49999);
  assert.sameValue(a[50000], "far");
  assert.sameValue(Object.keys(a).length, 50001);
})();

// Arrays around the minimum sparse length stay correct no matter how many holes they have.
(function () {
  var a = [];
  a[1023] = "end";
  assert.sameValue(a.length, 1024);
  assert.sameValue(a[1023], "end");
  assert.sameValue(Object.keys(a).join(","), "1023");
  assert.sameValue(0 in a, false);
  assert.sameValue(a[0], undefined);

  var b = [];
  b[1024] = "end";
  assert.sameValue(b.length, 1025);
  assert.sameValue(b[1024], "end");
  assert.sameValue(Object.keys(b).join(","), "1024");
  assert.sameValue(0 in b, false);
  assert.sameValue(b[0], undefined);
})();

// Growing a partially filled array across the density threshold preserves every property.
(function () {
  var a = [];
  for (var i = 0; i < 300; i++) {
    a[i] = i;
  }

  // Still filled enough to stay dense.
  a[2399] = "near";
  assert.sameValue(a.length, 2400);
  assert.sameValue(a[299], 299);
  assert.sameValue(a[2399], "near");
  assert.sameValue(Object.keys(a).length, 301);

  // Now far too empty to stay dense.
  a[24000] = "far";
  assert.sameValue(a.length, 24001);
  assert.sameValue(a[0], 0);
  assert.sameValue(a[299], 299);
  assert.sameValue(a[2399], "near");
  assert.sameValue(a[24000], "far");
  assert.sameValue(Object.keys(a).length, 302);
  assert.sameValue(300 in a, false);
})();

// Repeatedly crossing the dense and sparse thresholds keeps the array consistent.
(function () {
  var a = [];
  for (var round = 0; round < 3; round++) {
    a[20000] = "far";
    assert.sameValue(a.length, 20001);
    assert.sameValue(Object.keys(a).length, 1);

    for (var i = 0; i < 20000; i++) {
      a[i] = i;
    }
    assert.sameValue(a[0], 0);
    assert.sameValue(a[19999], 19999);
    assert.sameValue(a[20000], "far");
    assert.sameValue(Object.keys(a).length, 20001);

    a.length = 0;
    assert.sameValue(a.length, 0);
    assert.sameValue(Object.keys(a).length, 0);
    assert.sameValue(0 in a, false);
  }
})();

// A length beyond the maximum dense length is stored sparsely without presizing.
(function () {
  var a = [1, 2, 3];
  a.length = 20000000;
  assert.sameValue(a.length, 20000000);
  assert.sameValue(a[0], 1);
  assert.sameValue(a[2], 3);
  assert.sameValue(Object.keys(a).length, 3);
  assert.sameValue(1000000 in a, false);

  a[19999999] = "end";
  assert.sameValue(a[19999999], "end");
  assert.sameValue(a.length, 20000000);
  assert.sameValue(Object.keys(a).length, 4);
})();
