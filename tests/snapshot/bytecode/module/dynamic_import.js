import(1 + 2) + 3;

import(4, {});

function testResolveOptions() {
  const local = 1;
  import(4, { local });
}