class MyError {
  constructor(message) {
    this.message = message;
  }
}

1;
throw new MyError("foo");
2;