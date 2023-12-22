// Property with initializers

class C { a = b }

class C2 { 1 = b }

class C3 { "prop" = b }

class C4 { [key] = b }

class C5 { #key = b }

// Property without initializer

class C6 { a; b; c };

class C7 { 1 }

class C8 { "prop" }

class C9 { [key] }

class C10 { #key }

class C11 {
  a = b
  c = d
  e = f
}

// Methods

class C12 { a() {} }

class C13 { a(b,) {} }

class C14 { async a() {}; *a() {}; async *a() {} }

class C15 {
  [a]() {}
  1() {}
  "test"() {}
  #a() {}
}

// Keywords as shorthand properties
class C16 {
  get;
  set;
  async;
  static;
}

class C17 {
  get = 1
  set = 1
  async = 1
  static = 1
}

// Keywords as methods
class C18 {
  get() {}
  set() {}
  async() {}
  static() {}
}

class C19 {
  async get() {}
  async set() {}
  async async() {}
  async static() {}
}

class C20 {
  get a() {}
  set a(b) {}
}

class C21 {
  get [a]() {}
  get 1() {}
  get "a"() {}
  get #a() {}
}

class C22 {
  get get() {}
  get set() {}
  set get(x) {}
  set set(x) {}
}

// Static properties

class C23 {
  static a
  static a = 1
  static [a] = 1
  static #a = 1
  static a() {}
  static #a() {}
  static async a() {}
  static get a() {}
  static set a(b,) {}

  static static
  static static = 1
  static static() {}
}

