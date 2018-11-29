# coarse

Clojure lens library, useful for data manipulation.

## Getting Started

To install, add the following dependency to your project or build file:

```
[coarse "0.1.0-SNAPSHOT"]
```

```clojure
(ns example.example
  (:require [coarse.core :refer [view ix]]))

(println (view (ix 3) [1 2 3 4 5]))
```

See the [interactive guide](https://coarse-docs.netlify.com/) for more in depth information.

## Stability

While this library has been tested, do expect possible
bugs. I feel this library is stable for use in toy projects,
so obviously do not use this in production.

I will adhere to semver whenever possible, but expect breaking
changes.

## Examples


### Basics

```clojure
(view (ix 0) [:a :b :c :d])
; :a

(sett (ix 0) :yeah [:foo :b :c :d])
; [:yeah :b :c :d]

(over (ix 1) inc [1 2 3 4])
; [1 3 3 4]

(over (_index even?) inc [1 2 3 4])
; [2 2 4 4]

(over (_index odd?) inc [1 2 3 4])
; [1 3 3 5]

(sett (_taking 2) 99 [1 2 3 4])
; [99 99 3 4]

(to-list-of (_taking 2) [:a :b :c :d])
; (:a :b)

(to-list-of (*> each each) [[1] [2 3] [4 5] [6]])
; (1 2 3 4 5 6)
```


### Pseudo-Imperative Code

```clojure
(def game-state
  {:player {:pos {:x 0 :y 0} :hp 100 :inventory []}})

(view :x {:x 99 :z 3})
; 99
  
(->> game-state
  (+>> (*> :player :pos)
    (+= :x 1)
    (+= :y 1)))
; {:player {:pos {:x 1 :y 1}} :hp 100 :inventory []}
```

## Implementation

In hindsight, there was no apparent reason for me to use the van Laarhoven encoding for this library (aside from acting as a partial
reimplementation for micro-lens). I still believe the current encoding is better than the naive encoding (as a struct with a
function get and set), but I will try to explore more "clojure-ish" encodings of functional lenses. Currently the usages of
pseudo-typeclasses in the code are like paper-cuts that really annoys me.

## See Also


### [specter](https://github.com/nathanmarz/specter) 

Specter solves the same problem, but it is much more mature and stable. Use specter if you are interested in getting things done. Use this library if you are interested in toying with Haskell concepts.

### [traversy](https://github.com/ctford/traversy) 

Traversy is similar in nature, with a better encoding of lens, but fewer features than this library. This library is also closer to the original Haskell implementation.

Also I borrowed the ```*>``` notation from traversy.

## License

Copyright © 2018 RuneBlaze

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
