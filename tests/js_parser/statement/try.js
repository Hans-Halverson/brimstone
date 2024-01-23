throw a;

try {
  a;
} catch {
  b;
}

try {
  a;
} catch (b) {
  c;
}

try {
  a;
} finally {
  b;
}

try {
  a;
} catch {
  b;
} finally {
  c;
}