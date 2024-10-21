var proxy = new Proxy({}, {
  getOwnPropertyDescriptor() {
    throw new Error()
  },
  ownKeys() {
    return ['a']
  }
});

var { ...x } = proxy;