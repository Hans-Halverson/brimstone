async function foo() {
    await Promise.reject(42);
}
await foo();
