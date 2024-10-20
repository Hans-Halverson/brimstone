// Source location for where static initializer was called is start of class
class C {
  static {
    throw new Error();
  }
}