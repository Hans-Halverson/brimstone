/*---
description: Crash due to stack overflow error created when there was no current realm (i.e. no stack frames).
---*/

const COUNTER_LIMIT = 100;
const COUNTER_DONE_ERROR = new Error('CounterDone');

let counter = 0;

function inner1(target, key) {
    try {
        inner2(target, key);
    } catch (e) {
        if (e === COUNTER_DONE_ERROR) {
            throw e;
        }
    }
}

function inner2(target, key) {
    counter++;
    if (counter >= COUNTER_LIMIT) {
        throw COUNTER_DONE_ERROR;
    }

    Reflect.has(target, key);
}

function F1() {
    for (let v1 = 0; v1 < 25; v1++) {
        async function f7(value) {
            const newPrototype = new Proxy(Object.getPrototypeOf(value), {
                get(target, key, receiver) {
                    inner1(target, key);
                    return Reflect.get(target, key, receiver);
                },
                has(target, key) {
                    inner1(target, key);
                    return Reflect.has(target, key);
                },
            });

            Object.setPrototypeOf(value, newPrototype);

            await 1;

            return value;
        }
        
        f7(Number);
    }
}

new F1();