/*---
description: Class with computed method names, where computed name is a direct use of a binding.
---*/

function test(key2) {
  const key1 = "foo";

  class C {
    [key1]() {
      return 11;
    }
    [key2]() {
      return 12;
    }
  }

  const c = new C();

  assert.sameValue(c.foo(), 11);
  assert.sameValue(c.bar(), 12);
}

test("bar");