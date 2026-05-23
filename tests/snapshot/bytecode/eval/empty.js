// OPTIONS: --run

// Direct eval
eval(``);
eval(`"use strict";`);

// Indirect eval
eval?.(``);
eval?.(`"use strict";`);