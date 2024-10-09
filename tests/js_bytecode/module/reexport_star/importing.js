import { func1, var2 } from "./reexporting.js";

function testAccessImportedBindings() {
  func1 + var2;
}