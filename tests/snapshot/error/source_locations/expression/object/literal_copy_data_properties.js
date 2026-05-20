var proxy = new Proxy({}, {
  getOwnPropertyDescriptor() {
    throw new Error()
  },
  ownKeys() {
    return ['a']
  }
});

({ ...proxy });