/*---
description: Set operations handle resizing the set that is being iterated over.
---*/

function growingSetLike(x, visited, hasResult) {
  return {
    size: 1000,
    has(value) {
      visited.push(value);

      if (x.size < 100) {
        x.add(x.size);

        // Collect the set that was resized away, so that continuing to read from it causes a crash
        $262.gc();
      }

      return hasResult;
    },
    keys() {
      throw new Error();
    },
  };
}

function assertVisitedAllElements(x, visited) {
  assert.sameValue(x.size, 100);
  assert.sameValue(visited.length, 100);

  for (var i = 0; i < 100; i++) {
    assert.sameValue(visited[i], i);
  }
}

(function testGrowthWhileIteratingInIntersection() {
  var x = new Set([0, 1, 2, 3]);
  var visited = [];

  assert.sameValue(x.intersection(growingSetLike(x, visited, false)).size, 0);

  assertVisitedAllElements(x, visited);
})();

(function testGrowthWhileIteratingInIsDisjointFrom() {
  var x = new Set([0, 1, 2, 3]);
  var visited = [];

  assert.sameValue(x.isDisjointFrom(growingSetLike(x, visited, false)), true);

  assertVisitedAllElements(x, visited);
})();

(function testGrowthWhileIteratingInIsSubsetOf() {
  var x = new Set([0, 1, 2, 3]);
  var visited = [];

  assert.sameValue(x.isSubsetOf(growingSetLike(x, visited, true)), true);

  assertVisitedAllElements(x, visited);
})();
