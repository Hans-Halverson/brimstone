// Test all 8 combinations of init, test, and update
function testNoInitTestOrUpdate() {
  1;
  for (;;) {
    2;
  }
  3;
}

function testInitNoTestOrUpdate() {
  1;
  for (2;;) {
    3;
  }
  4;
}

function testNoInitTestNoUpdate() {
  1;
  for (; 2;) {
    3;
  }
  4;
}

function testNoInitOrTestUpdate() {
  1;
  for (;; 2) {
    3;
  }
  4;

}

function testInitTestNoUpdate() {
  1;
  for (2; 3;) {
    4;
  }
  5;
}

function testNoInitTestUpdate() {
  1;
  for (; 2; 3) {
    4;
  }
  5;
}
function testInitNoTestUpdate() {
  1;
  for (2;; 3) {
    4;
  }
  5;
}

function testInitTestUpdate() {
  1;
  for (2; 3; 4) {
    5;
  }
  6;
}

function testVarDeclInit() {
  1;
  for (var x = 2; 3; 4) {
    5;
  }
  6;
}