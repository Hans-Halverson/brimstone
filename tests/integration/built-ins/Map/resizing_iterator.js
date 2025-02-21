/*---
description: Map can be modified while iterating, causing underlying map to be resized.
---*/

(function testGrowthWhileIterating() {
  var x = new Map([['0', 0], ['1', 1], ['2', 2], ['3', 3]]);
  var i = 4;
  var visited = [];

  for (var [k, v] of x) {
    if (i < 100) {
      x.set(`${i}`, i)
    }

    visited.push([k, v]);

    i++;
  }

  for (var i = 0; i < 100; i++) {
    assert.sameValue(visited[i][0], `${i}`);
    assert.sameValue(visited[i][1], i);
  }
})();

(function testMultipleGrowthWhileIterating() {
  var x = new Map([['0', 0], ['1', 1], ['2', 2], ['3', 3]]);
  var i = 3;
  var visited = [];

  for (var [k, v] of x) {
    // Add many elements at once, growing map multiple times
    if (i == 4) {
      for (; i < 100; i++) {
        x.set(`${i}`, i);

        // Delete a few elements
        if (i % 10 === 0) {
          x.delete(`${i}`);
        }
      }
    }

    visited.push([k, v]);
    i++;
  }

  var i = 0;
  for (var j = 0; j < 100; j++) {
    if (j > 0 && j % 10 === 0) {
      continue;
    }

    assert.sameValue(visited[i][0], `${j}`);
    assert.sameValue(visited[i][1], j);

    i++;
  }
})();

(function testDeletingEarlierEntryBeforeGrowing() {
  var x = new Map([['0', 0], ['1', 1], ['2', 2], ['3', 3]]);
  var visited = [];

  var iter = x[Symbol.iterator]();
  visited.push(iter.next().value);
  
  // Delete before next index then grow
  x.delete('0');
  x.set('4', 4);
  x.set('5', 5);
  x.set('6', 6);

  for (var [k, v] of iter) {
    visited.push([k, v]);
  }

  for (var i = 0; i < 6; i++) {
    assert.sameValue(visited[i][0], `${i}`);
    assert.sameValue(visited[i][1], i);
  }
})();

(function testGrowthWithDeletionWhileIterating() {
  var x = new Map([['0', 0], ['1', 1], ['2', 2], ['3', 3]]);

  var i = 4;
  var visited = [];

  var j = 0;

  for (var [k, v] of x) {
    if (i < 100) {
      x.set(`${i}`, i);

      if (i % 2 === 0) {
        x.delete(`${j}`);
        j++;
      }
    }

    visited.push([k, v]);

    i++;
  }

  for (var i = 0; i < 100; i++) {
    assert.sameValue(visited[i][0], `${i}`);
    assert.sameValue(visited[i][1], i);
  }
})();