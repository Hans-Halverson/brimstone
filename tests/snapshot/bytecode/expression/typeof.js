function basic() {
  typeof 1;
  typeof typeof "a";
}

var x = 1;

function globalLookups() {
  var y = 2;

  // Resolved global
  typeof x;

  // Resolved local
  typeof y;

  // Unresolved global
  typeof z;
}

function dynamicLookups() {
  {
    eval('');
  }

  var y = 2;

  // Unresolved dynamic
  typeof x;

  // Resolved local
  typeof y;

  // Unresolved dynamic
  typeof z;
}