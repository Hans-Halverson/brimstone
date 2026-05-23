class C {
  get #foo() {
    throw new Error();
  }

  test() {
    this.#foo
  }
}

var c = new C();
c.test();