export var var1 = 1;
export let let1 = 2;
export const const1 = 3;
export class Class1 {}

// Initialized when linking, does not need to be initialized at runtime
export function func1() {}

function testAccessSimpleExportedBinding() {
  var1 + let1 + const1 + Class1 + func1;

  {
    // Create another scope
    const const2 = 4;
    function capturing() { const1 + const2 }

    // Access from nested scope
    capturing(const1);
  }
}

// Exported in a separate declaration
var var2 = 5;

// Exported under a different name
var var3 = 6;

export { var2, var3 as renamedVar3 };

function testAccessExportedInSeparateDeclaration() {
  // Renamed variable is not resolved
  var2 + var3 + renamedVar3;
}