class MyError {
  constructor(message) {
    this.name = "MyCustomError";
    this.message = message;
  }
}

1;
throw new MyError("foo");
2;