class C {
  set #foo(_) {
    throw new Error()
  }

  test() {
    this.#foo = 2;
  }
}

var c = new C();
c.test();