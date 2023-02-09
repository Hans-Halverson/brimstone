import 'a';

import * as a from 'b';


import a from 'b';

import a, * as b from 'c';

import a, { b } from 'c';


import {} from 'a';

import {a} from 'b';

import {a,} from 'b';

import {a, b, c} from 'd';

import {a as b, c, d as e} from 'f';

import {'a' as b, 'c' as d} from 'e';

import(a)

import(a);