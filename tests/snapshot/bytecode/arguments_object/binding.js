function bindingReferenced() {
  "use strict";
  arguments + 1;
}

function capturedByArrow() {
  "use strict";
  (() => arguments);
}