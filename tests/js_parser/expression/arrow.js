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