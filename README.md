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

See the interactive guide for more in depth information.

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
    (+= x 1)
    (+= y 1)))
; {:player {:pos {:x 1 :y 1}} :hp 100 :inventory []}
```

## License

Copyright © 2018 RuneBlaze

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
