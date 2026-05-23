import "mod1" with {}
import {} from "mod2" with {}

import "mod3" with { key: "prop" }
import "mod4" with { key: "prop", }
import "mod4" with { key: "prop", key2: "prop2" }
import "mod5" with { key: "prop", key2: "prop2", }

import "mod6" with { "a": "prop1", b: "prop2", import: "prop3" }

export * as ns from "mod7" with { key: "prop" }
export { foo } from "mod8" with { key: "prop" }