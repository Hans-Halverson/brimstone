// Force an OOM error
for (let i = 0; i < 1024 * 1024; i++) {
  new ArrayBuffer(1024 * 1024 * 1024);
}