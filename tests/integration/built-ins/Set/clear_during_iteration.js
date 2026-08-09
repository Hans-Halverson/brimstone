/*---
description: Set can be cleared while iterating, and values added afterwards are still iterated.
---*/

function assertVisited(visited, expected) {
  assert.sameValue(visited.length, expected.length);

  for (var i = 0; i < expected.length; i++) {
    assert.sameValue(visited[i], expected[i]);
  }
}

(function testAddAfterClearWhileIterating() {
  var x = new Set([0, 1, 2, 3]);
  var visited = [];

  x.forEach((e) => {
    visited.push(e);

    // Values added after the clear must not reuse the indices of the cleared values, which are
    // behind the iterator.
    if (e === 1) {
      x.clear();
      x.add(4);
    }
  });

  assertVisited(visited, [0, 1, 4]);
  assert.sameValue(x.size, 1);
})();

(function testAddManyAfterClearWhileIterating() {
  var x = new Set([0, 1, 2, 3, 4]);
  var visited = [];

  x.forEach((e) => {
    visited.push(e);

    // Add more values than were cleared, resizing the set
    if (e === 2) {
      x.clear();

      for (var i = 5; i < 11; i++) {
        x.add(i);
      }
    }
  });

  assertVisited(visited, [0, 1, 2, 5, 6, 7, 8, 9, 10]);
})();

(function testClearWithoutAddWhileIterating() {
  var x = new Set([0, 1, 2, 3]);
  var visited = [];

  x.forEach((e) => {
    visited.push(e);
    x.clear();
  });

  assertVisited(visited, [0]);
  assert.sameValue(x.size, 0);
})();

(function testClearWhileIteratingWithForOf() {
  var x = new Set([0, 1, 2, 3]);
  var visited = [];

  for (var e of x) {
    visited.push(e);

    if (e === 1) {
      x.clear();
      x.add(4);
    }
  }

  assertVisited(visited, [0, 1, 4]);
})();
