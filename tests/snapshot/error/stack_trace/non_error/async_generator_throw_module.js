async function* gen() {
    yield 1;
}
const g = gen();
await g.next();
await g.throw(42);
