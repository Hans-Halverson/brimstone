function testCatchNoParameter() {
  1;
  try {
    2;
  } catch {
    3;
  }
  4;
}

function testCatchWithParameter() {
  1;
  try {
    2;
  } catch (e) {
    3 + e;
  }
  4;
}

function testCatchDestructuring() {
  1;
  try {
    2;
  } catch ({a, b = 3}) {
    4;
  }
  5;
}

// function testFinallyOnly() {
//   1;
//   try {
//     2;
//   } finally {
//     3;
//   }
//   4;
// }

// function testCatchAndFinally() {
//   1;
//   try {
//     2;
//   } catch (e) {
//     3;
//   } finally {
//     4;
//   }
//   5;
// }