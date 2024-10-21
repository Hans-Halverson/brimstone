class C {
  get #foo() {
    throw new Error();
  }

  test() {
    this.#foo += 1;
  }
}

var c = new C();
c.test();