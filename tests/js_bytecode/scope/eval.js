// OPTIONS: --run

// Starting a scope
try {
  eval(`x; const x = 1`);
} catch {}

// Captured
eval(`const x = 1; function inner() { x }`);