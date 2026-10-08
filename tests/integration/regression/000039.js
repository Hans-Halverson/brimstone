/*---
description: >
  Assigning the result of an await or yield to a local binding clobbered the binding's value when
  the function resumed with an abrupt completion.
flags: [async]
---*/

function* syncThrow() {
  let a = 1;
  try {
    a = yield 0;
  } catch (e) {
    assert.sameValue(e, 5);
  }
  return a;
}

const it1 = syncThrow();
it1.next();
assert.sameValue(it1.throw(5).value, 1);

let observedInFinally;
function* syncReturn() {
  let a = 1;
  try {
    a = yield 0;
  } finally {
    observedInFinally = a;
  }
}

const it2 = syncReturn();
it2.next();
assert.sameValue(it2.return(42).value, 42);
assert.sameValue(observedInFinally, 1);

async function awaitRejected() {
  let a = 1;
  try {
    a = await Promise.reject(2);
  } catch (e) {
    assert.sameValue(e, 2);
  }
  return a;
}

async function* asyncThrow() {
  let a = 1;
  try {
    a = yield 0;
  } catch (e) {
    assert.sameValue(e, 5);
  }
  return a;
}

async function* asyncReturn() {
  let a = 1;
  try {
    a = yield 0;
  } finally {
    observedInFinally = a;
  }
}

awaitRejected()
  .then((a) => {
    assert.sameValue(a, 1);

    const it = asyncThrow();
    return it.next().then(() => it.throw(5));
  })
  .then((result) => {
    assert.sameValue(result.value, 1);
    assert.sameValue(result.done, true);

    observedInFinally = undefined;
    const it = asyncReturn();
    return it.next().then(() => it.return(42));
  })
  .then((result) => {
    assert.sameValue(result.value, 42);
    assert.sameValue(result.done, true);
    assert.sameValue(observedInFinally, 1);
  })
  .then($DONE, $DONE);
