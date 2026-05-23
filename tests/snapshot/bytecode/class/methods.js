class C {
  constructor() {}

  named1() {}

  "named2"() {}

  1() {}

  1n() {}

  [1 + 2]() {}

  get getter() {}

  set setter(v) {}

  get [3 + 4]() {}

  set [5 + 6](v) {}

  static staticMethod() {}

  static get getter() {}

  static set setter(v) {}

  static [1 + 2]() {}
}