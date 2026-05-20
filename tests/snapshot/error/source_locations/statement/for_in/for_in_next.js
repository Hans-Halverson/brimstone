var numHasCalls = 0;

var iter = new Proxy(
  { key: 1 },
  {
    has() {
      throw new Error()
    }
  }
);

for (var x in iter) {}