function startScope() {
  const x = 1;
  let y = 2;

  {
    // TDZ check needed, but separate VM locations
    const x = x;
    let y = y;
  }
}