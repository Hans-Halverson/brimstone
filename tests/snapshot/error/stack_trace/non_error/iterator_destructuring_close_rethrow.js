const iter = {
    [Symbol.iterator]() {
        return {
            next() { return { value: [1, 2], done: false }; },
            return() {
                throw 42;
            }
        };
    }
};

const [a, b] = iter;
