# tech.jna

[![Clojars Project](https://img.shields.io/clojars/v/techascent/tech.jna.svg)](https://clojars.org/techascent/tech.jna)

For a bit of explanation, checkout our [blog](http://techascent.com/blog/jna-simplifies-your-life.html).


## Usage

```clojure
(require '[tech.jna :as jna])
(jna/def-jna-fn "c" memset
        "Set byte memory to a value"
        com.sun.jna.Pointer ;;void* return value
        [data identity]     ;;Each argument has a coercer-fn. Pointers can be lots of types.
        [value int]         ;;read docs for memset
        [n-bytes jna/size-t])
        
user> (def test-ary (float-array [1 2 3 4]))
#'user/test-ary
user> (vec test-ary)
[1.0 2.0 3.0 4.0]
user> (memset test-ary 0 (* 4 Float/BYTES))
user> (vec test-ary)
[0.0 0.0 0.0 0.0]
```

Copyright © 2018 TechAscent, LLC

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
