// OPTIONS: --run

// Starting a scope
try {
  eval(`x; const x = 1`);
} catch {}

// Captured
eval(`const x = 1; function inner() { x }`);

// Outer eval has lexical scope
eval(`eval('')`);

// Outer eval has lexical scope
eval(`"use strict"; eval('')`)

// Outer eval has global scope, not explicitly pushed
eval?.(`eval('')`);

// Outer eval has lexical scope
eval?.(`"use strict"; eval('')`);