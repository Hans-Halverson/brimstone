/*---
description: Bug where breaking out of a switch with a materialiezed VM scope would try to pop too many scopes and crash
---*/

switch (1) {
  case 1:
    let x = 1;
    function captures() { x }
    break;
}