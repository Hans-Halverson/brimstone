// Property without initializers

class C { a = b }

class C { 1 = b }

class C { "prop" = b }

class C { [key] = b }

// Property without initializer

class C { a; b; c };

class C { 1 }

class C { "prop" }

class C { [key] }

class C {
  a = b
  c = d
  e = f
}

// Methods

class C { a() {} }

class C { a(b,) {} }

class C { async a() {}; *a() {}; async *a() {} }

class C {
  [a]() {}
  1() {}
  "test"() {}
}

// Keywords as shorthand properties
class C {
  get;
  set;
  async;
  static;
}

class C {
  get = 1
  set = 1
  async = 1
  static = 1
}

// Keywords as methods
class C {
  get() {}
  set() {}
  async() {}
  static() {}
}

class C {
  async get() {}
  async set() {}
  async async() {}
  async static() {}
}

class C {
  get a() {}
  set a(b) {}
}

class C {
  get [a]() {}
  get 1() {}
  get "a"() {}
}

class C {
  get get() {}
  get set() {}
  set get(x) {}
  set set(x) {}
}

// Static properties

class C {
  static a
  static a = 1
  static [a] = 1
  static a() {}
  static async a() {}
  static get a() {}
  static set a(b,) {}

  static static
  static static = 1
  static static() {}
}

