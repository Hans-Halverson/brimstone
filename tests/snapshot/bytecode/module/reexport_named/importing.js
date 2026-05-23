import { func1, var2, reexport1 } from "./reexporting.js";

function testAccessImportedBindings() {
  func1 + var2 + reexport1;
}