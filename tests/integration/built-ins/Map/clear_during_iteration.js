/*---
description: Map can be cleared while iterating, and values added afterwards are still iterated.
---*/

function assertVisited(visited, expected) {
  assert.sameValue(visited.length, expected.length);

  for (var i = 0; i < expected.length; i++) {
    assert.sameValue(visited[i], expected[i]);
  }
}

(function testAddAfterClearWhileIterating() {
  var x = new Map([['0', 0], ['1', 1], ['2', 2], ['3', 3]]);
  var visited = [];

  x.forEach((v, k) => {
    visited.push(k);

    // Entries added after the clear must not reuse the indices of the cleared entries, which are
    // behind the iterator.
    if (k === '1') {
      x.clear();
      x.set('4', 4);
    }
  });

  assertVisited(visited, ['0', '1', '4']);
  assert.sameValue(x.size, 1);
})();

(function testAddManyAfterClearWhileIterating() {
  var x = new Map([['0', 0], ['1', 1], ['2', 2], ['3', 3], ['4', 4]]);
  var visited = [];

  x.forEach((v, k) => {
    visited.push(k);

    // Add more entries than were cleared, resizing the map
    if (k === '2') {
      x.clear();

      for (var i = 5; i < 11; i++) {
        x.set(`${i}`, i);
      }
    }
  });

  assertVisited(visited, ['0', '1', '2', '5', '6', '7', '8', '9', '10']);
})();

(function testClearWithoutAddWhileIterating() {
  var x = new Map([['0', 0], ['1', 1], ['2', 2], ['3', 3]]);
  var visited = [];

  x.forEach((v, k) => {
    visited.push(k);
    x.clear();
  });

  assertVisited(visited, ['0']);
  assert.sameValue(x.size, 0);
})();

(function testClearWhileIteratingWithForOf() {
  var x = new Map([['0', 0], ['1', 1], ['2', 2], ['3', 3]]);
  var visited = [];

  for (var [k, v] of x) {
    visited.push(k);

    if (k === '1') {
      x.clear();
      x.set('4', 4);
    }
  }

  assertVisited(visited, ['0', '1', '4']);
})();
