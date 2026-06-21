class MyError extends Error {
  constructor(message) {
    super(message);
    this.name = "MyError";
  }
}

1;
throw new MyError("foo");
2;