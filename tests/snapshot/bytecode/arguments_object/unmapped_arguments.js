function strictForcesUnmapped() {
  "use strict";
  arguments;
}

function defaultForcesUnmapped(x = 1) {
  arguments;
}

function destructuringForcesUnmapped({ x }) {
  arguments;
}

function restForcesUnmapped(...x) {
  arguments;
}

function withArguments(x, y, z) {
  "use strict";
  arguments;
}