// Parse in toplevel position
import.meta;
import.meta

// Parse when start of expression
import.meta(1);
import.meta + 1;
import.meta.foo;

function test() {
  // Parse in expression position
  import.meta;

  // Meta properties are allowed as MemberExpressions
  new import.meta;
}