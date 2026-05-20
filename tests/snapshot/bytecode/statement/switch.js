function empty() {
  1;
  switch (true) {}
  2;
}

function singleCaseWithFallthrough() {
  1;
  switch (true) {
    case 2:
      3;
  }
  4;
}

function singleCaseWithBreak() {
  1;
  switch (true) {
    case 2:
      3;
      break;
  }
  4;
}

function multipleCases() {
  1;
  switch (true) {
    case 2:
      3;
      break;
    case 4:
      5;
    case 6:
      7;
      break;
  }
  3;
}

function singleDefaultCase() {
  1;
  switch (true) {
    default:
      2;
  }
  3;
}

function multipleCasesWithDefault() {
  1;
  switch (true) {
    case 2:
      3;
      break;
    case 4:
      5;
      break;
    default:
      6;
      break;
  }
  7;
}

function multipleCasesWithDefaultInMiddle() {
  1;
  switch (true) {
    case 2:
      3;
      break;
    default:
      4;
      break;
    case 5:
      6;
      break;
  }
  7;
}

function multipleCasesWithDefaultInMiddleFallthrough() {
  1;
  switch (true) {
    case 2:
      3;
    default:
      4;
    case 5:
      6;
  }
  7;
}