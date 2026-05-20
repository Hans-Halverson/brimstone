var poisoned = new Proxy({}, {
  ownKeys() {
    throw new Error()
  }
});

for (var x in poisoned) {}