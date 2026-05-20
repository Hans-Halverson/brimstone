function simpleSpread(p) {
  return f(...p);
}

function spreadWithArgs(p) {
  return f(1, ...p);
}

function complexSpread() {
  return f(1, ...(2), 3, ...(4))
}

function spreadWithReceiver(o, p) {
  return o.f(...p)
}

function maybeEvalSpread(p) {
  return eval(...p)
}