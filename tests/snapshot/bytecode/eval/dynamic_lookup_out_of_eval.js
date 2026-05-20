// OPTIONS: --run

// Dynamic lookup for unresolved bindings out of eval
try {
  eval(`x; x = 1`);
} catch {}

// Dynamic lookup for var bindings since they are placed in parent scope
eval(`var x = 1; x;`);

// No dynamic lookup for var bindings in strict direct eval
eval(`'use strict'; var x = 1; x;`);

// No dynamic lookup for let/const bindings
eval(`let x = 1; const y = 2; x + y;`);

// Dynamic lookup for var function declarations but not lex
eval(`function varFunc() { return 1 }; { function lexFunc() { return 2 } }`);

// Global lookup for unresolved bindings in indirect eval
try {
  eval?.(`x; x = 1`);
} catch {}

// Dynamic lookup for var bindings in indirect eval since they are placed in parent scope
eval?.(`var x = 1; x;`);

// No dynamic lookup for var bindings in strict indirect eval
eval?.(`'use strict'; var x = 1; x;`);

// Dynamic lookup of new.target
(function() { eval('new.target') })();