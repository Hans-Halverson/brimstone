class C {
  // Constructors
  constructor() {}
  "constructor"() {}

  // Parsed as constructors, but will error during analysis
  async constructor() {}
  *constructor() {}
  async *constructor() {}

  // Not constructors
  constructor
  static constructor() {}
  [constructor]() {}
  ["constructor"]() {}
  get constructor() {}
  set constructor(a) {}
}