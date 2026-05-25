const str = "a".repeat(2 ** 16);
let sum = 0;
for (let i = 0; i < 2 ** 16; i++) {
    sum += str;
}