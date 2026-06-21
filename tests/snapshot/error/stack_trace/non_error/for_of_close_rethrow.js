const iter = {
    [Symbol.iterator]() {
        return {
            next() { return { value: 1, done: false }; },
            return() {
                throw 42;
            }
        };
    }
};

for (const x of iter) {
    break;
}
