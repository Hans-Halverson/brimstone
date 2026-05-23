await (Promise.resolve()).then(() => {
  throw new Error('test');
})