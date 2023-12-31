export {};
export {} from 'a';

export {a}
export {a,}
export {a, b, c}
export {a, b, c,}
export {a} from 'b'

export {a as b, c, d as e}

export {a as 'b'};

export var a = 1;
export let a2 = 1;
export const a3 = 1;
export const [a4, b5] = 1;

export class C {};

export function foo() {}
export async function foo2() {}

export default function foo3() {}
export default function () {}
export default async function foo4() {}
export default async function () {}

export default class C2 {}
export default class {}

export default a6;
export default 1 + 2;

export * from 'other';
export * as a from 'other';
export * as 'a' from 'other';