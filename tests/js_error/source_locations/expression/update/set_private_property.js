class C {
  get #foo() {
    return 1;
  }

  set #foo(_) {
    throw new Error()
  }

  test() {
    this.#foo++;
  }
}

var c = new C();
c.test();