/*---
description: Set can be modified while iterating, causing underlying map to be resized.
---*/

(function testGrowthWhileIterating() {
  var x = new Set([0, 1, 2, 3]);
  var i = 4;
  var visited = [];

  for (var e of x) {
    if (i < 100) {
      x.add(i)
    }

    visited.push(e);

    i++;
  }

  for (var i = 0; i < 100; i++) {
    assert.sameValue(visited[i], i);
  }
})();

(function testMultipleGrowthWhileIterating() {
  var x = new Set([0, 1, 2, 3]);
  var i = 3;
  var visited = [];

  for (var e of x) {
    // Add many elements at once, growing map multiple times
    if (i == 4) {
      for (; i < 100; i++) {
        x.add(i);

        // Delete a few elements
        if (i % 10 === 0) {
          x.delete(i);
        }
      }
    }

    visited.push(e);
    i++;
  }

  var i = 0;
  for (var j = 0; j < 100; j++) {
    if (j > 0 && j % 10 === 0) {
      continue;
    }

    assert.sameValue(visited[i], j);

    i++;
  }
})();

(function testDeletingEarlierEntryBeforeGrowing() {
  var x = new Set([0, 1, 2, 3]);
  var visited = [];

  var iter = x[Symbol.iterator]();
  visited.push(iter.next().value);
  
  // Delete before next index then grow
  x.delete(0);
  x.add(4);
  x.add(5);
  x.add(6);

  for (var e of iter) {
    visited.push(e);
  }

  for (var i = 0; i < 6; i++) {
    assert.sameValue(visited[i], i);
  }
})();

(function testGrowthWithDeletionWhileIterating() {
  var x = new Set([0, 1, 2, 3]);

  var i = 4;
  var visited = [];

  var j = 0;

  for (var e of x) {
    if (i < 100) {
      x.add(i);

      if (i % 2 === 0) {
        x.delete(j);
        j++;
      }
    }

    visited.push(e);

    i++;
  }

  for (var i = 0; i < 100; i++) {
    assert.sameValue(visited[i], i);
  }
})();