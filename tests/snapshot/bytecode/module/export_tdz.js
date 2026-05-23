// No TDZ check needed
export let x1 = 1;
// TDZ check needed
x1 = 2;

function testAccessWithTDZ() {
  x1 = 3;
}

// TDZ check needed
x2 = 4;
// No TDZ check needed
export let x2 = 5;