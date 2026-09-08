enum First { First(word) }
enum Second { Second }

export mod;
export mod as M;
export mod.{a};
export { T(*) };

import {T} from m;
