class C {
  set #foo(_) {
    throw new Error()
  }

  test() {
    ({ x: this.#foo } = { x: 1 });
  }
}

var c = new C();
c.test();