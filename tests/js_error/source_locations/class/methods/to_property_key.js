var poisoned = { toString() { throw new Error() } };

class C {
  [poisoned]() {}
}