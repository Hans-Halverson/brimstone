"use strict";

// Not a use strict directive
"use strict";

function foo() {
  "use strict";
  1;
}

function foo() {
  'use strict';
  1;
}

const a = () => {
  "use strict";
  1;
};

({
  method() {
    "use strict";
    1;
  }
});

// ASI
function foo() {
  "use strict"
  1;
}

// Not a use strict directive
function foo() {
  1;
  "use strict";
}

function foo() {
  "use stri\u{0063}t";
}