function* gen() {
    yield 1;
}
const g = gen();
g.next();
g.throw(42);
