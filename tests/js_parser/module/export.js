export {};
export {} from 'a';

export {a}
export {a,}
export {a, b, c}
export {a, b, c,}
export {a} from 'b'

export {a as b, c, d as e}

export {'a' as 'b', 'c'};

export var a = 1;
export let a = 1;
export const a = 1;
export const [a, b] = 1;

export class C {};

export function foo() {}
export async function foo() {}

export default function foo() {}
export default function () {}
export default async function foo() {}
export default async function () {}

export default class C {}
export default class {}

export default a;
export default 1 + 2;

export * from 'other';
export * as a from 'other';
export * as 'a' from 'other';