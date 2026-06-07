/*---
description: >
  Parse the "WSpace" alias for the White_Space unicode binary property. Non-standard but this
  appears to be an oversight and the browser engines support this alias. 
---*/

assert(/\p{WSpace}/u.test(' ')) 