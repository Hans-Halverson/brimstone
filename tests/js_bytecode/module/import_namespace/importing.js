// Should be stored in scope
import * as import1 from './exporting.js';

// Should be stored in module scope
import * as import2 from './exporting.js';
export { import2 }

function testAccess() {
  import1;
  import2;
}