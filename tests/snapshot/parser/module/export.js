export {};
export {} from 'a';

export {a}
export {a,}
export {a, b, c}
export {a, b, c,}
export {a} from 'b'

export {a as b, c, d as e}

// Strings and reserved words allowed as exported name
export {a as 'b'};
export {a as if};

// Strings and reserved words are allowed as both local and exported names when `from` clause is present
export {'a' as 'b'} from 'other';
export {if as if} from 'other';

export var a = 1;
export let a2 = 1;
export const a3 = 1;
export const [a4, b5] = 1;

export class C {};

export function foo() {}
export async function foo2() {}

export default a6;

export * from 'other';
export * as a from 'other';
export * as 'a' from 'other';