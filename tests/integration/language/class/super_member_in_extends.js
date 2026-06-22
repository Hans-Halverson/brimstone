/*---
description: Super member expressions in extends clauses.
---*/

class Base {
  class() { return Right; }
  static class() { return RightStatic; }
}

class Right {}
class Wrong {}
class RightStatic {}

// Super references parent class
class C1 extends Base {
  class() { return Wrong; }

  method() {
    return class extends (super.class()) {}
  }
}

const class1 = (new C1()).method();
assert(new class1() instanceof Right);

// Super in arrow function references parent class
class C2 extends Base {
  class() { return Wrong; }

  method() {
    return class extends (() => super.class())() {}
  }
}

const class2 = (new C2()).method();
assert(new class2() instanceof Right);

// Super in static method references parent class
class C3 extends Base {
  class() { return Wrong; }

  static method() {
    return class extends (super.class()) {}
  }
}

const class3 = C3.method();
assert(new class3() instanceof RightStatic);

// Super in method reference parent object
const object = {
  __proto__: { prop: Right },
  method() {
    return class extends (super.prop) {}
  }
};

const class4 = object.method();
assert(new class4() instanceof Right);
