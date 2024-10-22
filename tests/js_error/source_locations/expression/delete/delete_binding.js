var obj = new Proxy(
  { foo: 1 },
  {
    has() {
      throw new Error()
    }
  }
);

with (obj) {
  delete foo;
}