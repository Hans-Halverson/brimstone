/*---
description: Completion values of directives.
---*/

assert.sameValue(eval("'use strict'"), "use strict");
assert.sameValue(eval("'use strict'; var x = 1;"), "use strict");
assert.sameValue(eval("'use strict'; function f() {}"), "use strict");
assert.sameValue(eval("'foo'; 'use strict'; { }"), "use strict");
assert.sameValue(eval("'use strict'; 'foo'; var x;"), "foo");
assert.sameValue(eval("'use strict'; 1"), 1);
assert.sameValue(eval("'use strict'; if (true) {}"), undefined);
