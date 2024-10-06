import { named1 } from './import_named_1.js';
import { named2, named3 } from './import_named_1.js';
import { named1 as renamed1 } from './import_named_1.js';

function testAccess() {
  named1 + named2 + named3 + renamed1;

  {
    // Create a nested scope
    const c = 0;
    function capturing() { named1 + c }

    // Acces from nested scope
    capturing(named1);
  }
}