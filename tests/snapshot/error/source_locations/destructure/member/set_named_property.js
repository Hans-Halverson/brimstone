var obj = {
  set foo(_) {
    throw new Error();
  }
};

({ x: obj.foo } = { x: 1 });