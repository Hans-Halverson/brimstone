import { func1, var2 } from "./exporting.js";

function testAccessImportedBindings() {
  func1 + var2;
}