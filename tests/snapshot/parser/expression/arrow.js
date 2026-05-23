() => 1;

() => {};

(x) => {};

(x,) => {};

(x, y, z) => {};

(x, y, z,) => {};

x => {};

async => {};

async () => {};

async (x) => {};

async (x, y) => {};

async x => {};

// Call, not async arrow expression
async();

// Await allowed within async arrow function
async () => { await 1; };

// Yield not allowed within arrow functions
function* f(){
  () => yield
}

function* f(){
  async => yield
}

function *f() {
  () => {
    yield;
  }
}

function* f() {
  () => yield* 1;
}