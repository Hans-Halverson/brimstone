export var var1 = 1;
export let let1 = 2;
export const const1 = 3;
export class Class1 {}

// Initialized when linking, does not need to be initialized at runtime
export function func1() {}

function testAccessExportedBinding() {
  var1 + let1 + const1 + Class1 + func1;

  {
    // Create another scope
    const const2 = 4;
    function capturing() { const1 + const2 }

    // Access from nested scope
    capturing(const1);
  }
}