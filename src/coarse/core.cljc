(ns coarse.core
  "defines all the lenses and traversals"
  (:require [coarse.cats :refer
             [pure pure? mk-identity
              mk-const mk-first mk-tuple
              just nothing nothing?
              cons' lift-pure
              fmap app traverse
              mempty mappend pure-from]]))

(defn &
  "reverse function application, not useful in Clojure yet"
  [x f] (f x))

(defn lens
  "constructs a lens from a getter and setter"
  [get set]
  ; get :: (s -> a)
  ; set :: (s -> b -> t)
  (let [g (fn [f s]
            ; f : (a -> f b)
            (let [a (get s)
                  fb (f a)
                  b->t (partial set s)]
              (fmap b->t fb)))]
    (fn
      ([x] (partial g x))
      ([x y] (g x y)))))

(defn _pred
  "traverses all elements, only access them when its index is satisfied by p1, value satisfied by p2"
  [p1 p2]
  (letfn [(g [f s']
            (let [s (map-indexed
                      vector s')
                  hint (:type (f (first s')))]
              (letfn [(upd [[ind ele]]
                        (if (and (p1 ind) (p2 ele))
                          (f ele)
                          (pure-from hint ele)))]
                (traverse upd s))))]
    (fn
      ([a b] (g a b))
      ([a] (partial g a)))))

(def _index
  "traverses all elements, access those whose index satisfied by the predicate"
  (fn [x] (_pred x (constantly true))))

(def _filtering
  "traverses all elements, access those whose value satisfied by the predicate"
  (partial _pred (constantly true)))

(defn _taking [n]
  "traverses all elements, access those within (take n)"
  (_index #(< % n)))

(defn _dropping [n]
  "traverses all elements, access those within (drop n)"
  (_index #(>= % n)))

(defn _ranging [coll]
  "traverses all elements, access those within the range"
  (_index #(some #{%} coll)))

(defn _all [v]
  "traverses all elements, access those equal to v"
  (_filtering (partial = v)))

(defn ix [k]
  "takes a key and constructs a lens, using get and assoc"
  (let [get #(get % k)
        set (fn [s b]
              (assoc (doall s) k b))]
    (lens get set)))

(defn ix-default [n default]
  "ix variant that takes a default value on failure"
  (let [get (fn [x] (get x n default))
        set (fn [s b] (assoc s n b))]
    (lens get set)))

(defn nx [n]
  "takes a numeric key and constructs a lens, using nth and assoc"
  (let [get #(nth % n)
        set (fn [s b]
              (assoc (doall s) n b))]
    (lens get set)))

(defn- wrap-lens [x]
  (if (or (keyword? x) (integer? x))
    (ix x)
    x))

(defn preview [lns s]
  "uses the traversal on the structure, and returns a maybe value based on the first element it accesses"
  (let [v (:value (:value ((wrap-lens lns) (fn [x] (mk-const (mk-first (just x)))) s)))]
    (if (nil? v)
      nothing
      v)))

(defn has? [lns s]
  "tests if the lens actually access any value on the structure, buggy at this moment"
  (let [v (preview lns s)]
    (if (or (nil? v) (nothing? v))
      false
      true)))

(defn filtered [pred]
  "access a value if it satisfies the predicate"
  (letfn [(g [f s]
            (let [hint (:type (f s))]
              (if (pred s)
                (f s)
                (pure-from hint s))))]
    (fn
      ([f] (partial g f))
      ([f s] (g f s)))))

(def _1
  "shorthand for (ix 0)"
  (ix 0))

(def _2
  "shorthand for (ix 1)"
  (ix 1))

(def _3
  "shorthand for (ix 2)"
  (ix 2))

(def _4
  "shorthand for (ix 3)"
  (ix 3))

(def _5
  "shorthand for (ix 4)"
  (ix 4))

(defn <%- [lns f s]
  "over variant that also returns the changed value"
  (:value
    (lns (fn [x]
           (let [fx (f x)]
             (mk-tuple fx fx))) s)))

(defn <<%- [lns f s]
  "over variant that also returns the old value"
  (:value
    (lns (fn [x] (mk-tuple x (f x))) s)))

(def _first
  "alias for _1"
  _1)

(def _last
  "lens that access the last element of a collection"
  (lens
    last
    (fn [x v] (assoc x (dec (count x)) v))))

(def _rest
  "lens that access the rest of a collection"
  (lens
    rest
    (fn [[hd & _] v] (cons hd v))))

(def _butlast
  "lens that access the butlast of a collection"
  (lens
    butlast
    (fn [coll v] (conj v (last coll)))))

(defn hcomp2 [f g]
  "function composition suitable for lens composition, mimics the Haskell behavior"
  (fn [hd & rst_]
    (let [rst (if (nil? rst_) [] rst_)]
      (apply (partial f (g hd)) rst))))

(defn hcomp [& fns']
  "function composition suitable for lens composition; auto converts integers and keywords to ix lenses"
  (let [fns (map wrap-lens fns')]
    (cond
      (empty? fns) identity
      (= (count fns) 1) (first fns)
      :else
      (let [b (last fns)
            a (apply hcomp (butlast fns))]
        (hcomp2 a b)))))

(defn attrs [& form]
  "converts all its arguments into ix lenses, and then compose them"
  (apply hcomp (map ix form)))

(defn over [lns a->b s]
  "update-in for lens"
  (:value ((wrap-lens lns) (comp mk-identity a->b) s)))

(defn sett [lns b s]
  "assoc-in for lens"
  (over lns (constantly b) s))

(defn view [lns s]
  "get-in for lens"
  (:value ((wrap-lens lns) mk-const s)))

(defn to [getter]
  "converts a getter function into lens that can only get"
  (lens getter (constantly nil)))

(defn views
  "performs the function within the context pointed by the lens"
  ([l f] (partial view (hcomp l (to f))))
  ([l] (partial views l)))

(defn join [& lenses]
  "joins the views of lenses into a new lens as a vector"
  (lens
    (fn [s]
      (mapv #(view % s) lenses))
    (fn [s b]
      ; s is the original structure
      ; b is a list of "inputs" for the lenses to set
      (letfn [(g [s'
                  [lh & ls]                                 ;; the lenses
                  [bh & bs]]                                ;; the inputs
                (if (nil? lh)
                  s'
                  (g
                    (sett lh bh s')
                    ls bs)))]
        (g s lenses b)))))

; FIXME: hack to force nil value to become a list
(defn to-list-of
  "view for traversals, combines the elements the traversal accesses"
  ([lns s]
   (let [v (:value ((wrap-lens lns) (fn [x] (mk-const [x])) s))]
     (if (nil? v) [] v)))
  ([lns] (partial to-list-of lns)))

;; maths operators

(defn ?= [fn]
  (letfn [(anon
            ([l n s] (over l (partial fn n) s))
            ([l n] (partial anon l n)))]
    anon))

(def +=
  (?= +))
(def -= (?= (fn [a b] (- b a))))
(def *= (?= *))
(def div= (?= (fn [a b] (/ b a))))
(def quot= (?= (fn [a b] (quot b a))))

(def =%
  "alias for sett"
  sett)

;; states

(defn state [f]
  {:value f :type :state})

(defn run-state
  ([v] (:value v))
  ([v arg] ((:value v) arg)))

(defn exec-state [a b]
  (second (run-state a b)))

(defn eval-state [a b]
  (first (run-state a b)))

(defn state-return
  ([x] (state (fn [s] [x s])))
  ([x s] [x s]))

(defn bind [p k]
  (state
    (fn [s0]
      (let [[x s1] (run-state p s0)]
        (run-state (k x) s1)))))

(defn chain [p k]
  (bind p (fn [_] k)))

(def state-get
  (state (fn [s] [s s])))

(defn state-put
  ([x] (state (fn [_] [[] x])))
  ([x s] [[] x]))

(defn state-modify [f]
  (bind state-get (fn [x] (state-put (f x)))))

(defn state-gets [f]
  (bind state-get (fn [x] (state-return (f x)))))

(defn state-view [lns]
  (bind state-get
        (fn [s] (state-return (view lns s)))))

(defmacro lens-do
  [& forms]
  (let [hd (first forms)
        tl (rest forms)]
    (if (coll? hd)
      ; a collection
      (if (empty? tl)
        hd
        (cond
          (= (first hd) '<=)
          `(bind (state-view ~(nth hd 2))
                 (fn [~(second hd)]
                   (lens-do ~@tl)))
          (= (first hd) '<-)
          `(bind ~(nth hd 2)
                 (fn [~(second hd)]
                   (lens-do ~@tl)))
          :else
          `(chain ~hd (lens-do ~@tl)))))))

(def magnify
  "alias for views"
  views)

(defn zoom [lns state]
  (lens-do
    (<= ele lns)
    (<- orig state-get)
    (state-put (sett lns (exec-state state ele) orig))))

;; state operators

(defn ?== [f]
  (fn [l b] (state-modify (f l b))))

(defn =$ [l b] (state-modify (partial sett l b)))
(def +== (?== +=))
(def -== (?== -=))
(def *== (?== *=))
(def div== (?== div=))
(def quot== (?== quot=))

;; aliases
(def *> hcomp)
(def *- attrs)
(def each traverse)
