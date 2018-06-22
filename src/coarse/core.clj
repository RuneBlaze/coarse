(ns coarse.core
  (:require [coarse.cats :refer
             [pure pure? mk-identity
              mk-const mk-first mk-tuple
              just nothing nothing?
              cons' lift-pure
              fmap app traverse
              mempty mappend]]))

(defn lens [get set]
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

(defn _pred [p1 p2]
  (letfn [(g [f s']
            (let [s (map-indexed
                      vector s')
                  hint (:type (f (first s')))]
              (letfn [(upd [[ind ele]]
                        (if (and (p1 ind) (p2 ele))
                          (f ele)
                          (case hint
                            :const (mk-const [])
                            :identity (mk-identity ele)
                            :first (mk-first (just ele))
                            (pure ele))))]
                (traverse upd s))))]
    (fn
      ([a b] (g a b))
      ([a] (partial g a)))))

(def _index (fn [x] (_pred x (constantly true))))
(def _filtering (partial _pred (constantly true)))
(defn _taking [n] (_index #(< % n)))
(defn _dropping [n] (_index #(>= % n)))
(defn _ranging [coll] (_index #(some #{%} coll)))
(defn _all [v] (_filtering (partial = v)))

(defn ix [n]
  (let [get #(nth % n)
        set (fn [s b]
              (assoc (apply vector s) n b))]
    (lens get set)))

(defn preview [lns s]
  (let [v (-> (lns (fn [x] (mk-const (mk-first (just x)))) s)
              :value
              :value)]
    (if (nil? v)
      nothing
      v)))

(defn has? [lns s]
  (let [v (preview lns s)]
    (if (or (nil? v) (nothing? v))
      false
      true)))

(def _1 (ix 0))
(def _2 (ix 1))
(def _3 (ix 2))
(def _4 (ix 3))
(def _5 (ix 4))

(defn <%- [lns f s]
  (:value
    (lns (fn [x]
           (let [fx (f x)]
             (mk-tuple fx fx))) s)))

(defn <<%- [lns f s]
  (:value
    (lns (fn [x] (mk-tuple x (f x))) s)))

(def _first _1)
(def _last
  (lens
    last
    (fn [x v] (assoc x (dec (count x)) v))))

(def _rest
  (lens
    rest
    (fn [[hd & _] v] (cons hd v))))

(def _butlast
  (lens
    butlast
    (fn [coll v] (conj v (last coll)))))

(defn attr [n]
  (let [get #(get % n)
        set (fn [s b] (assoc s n b))]
    (lens get set)))

(defn attr-default [n default]
  (let [get (fn [x] (or (get x n) default))
        set (fn [s b] (assoc s n b))]
    (lens get set)))

(defn hcomp2 [f g]
  (fn [hd & rst_]
    (let [rst (if (nil? rst_) [] rst_)]
      (apply (partial f (g hd)) rst))))

(defn wrap-lens [x]
  (if (keyword? x)
    (attr x)
    (if (integer? x)
      (attr x)
      x)))

(defn hcomp [& fns']
  (let [fns (map wrap-lens fns')]
    (cond
      (empty? fns) identity
      (= (count fns) 1) (first fns)
      :else
      (let [b (last fns)
            a (apply hcomp (butlast fns))]
        (hcomp2 a b)))))

(defn attrs [& form]
  (apply hcomp (map attr form)))

(defn over [lns a->b s]
  (:value (lns (comp mk-identity a->b) s)))

(defn sett [lns b s]
  (over lns (constantly b) s))

(defn & [f n] (n f))

(defn view [lns s]
  (:value (lns mk-const s)))

(defn to [getter]
  (lens getter (constantly nil)))

(defn views
  ([l f] (partial view (hcomp l (to f))))
  ([l] (partial views l)))

(defn join [& lenses]
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
  ([lns s]
   (let [v (:value (lns (fn [x] (mk-const [x])) s))]
     (if (nil? v) [] v)))
  ([lns] (partial to-list-of lns)))

;; maths operators

(defn ?% [fn]
  (letfn [(anon
            ([l n s] (over l (partial fn n) s))
            ([l n] (partial anon l n)))]
    anon))

(def +% (?% +))
(def -% (?% (fn [a b] (- b a))))
(def *% (?% *))
(def div% (?% (fn [a b] (/ b a))))
(def quot% (?% (fn [a b] (/ b a))))

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

(def magnify views)

(defn zoom [lns state]
  (lens-do
    (<= ele lns)
    (<- orig state-get)
    (state-put (sett lns (exec-state state ele) orig))))


;; state operators

(defn ?= [f]
  (fn [l b] (state-modify (f l b))))

(defn =% [l b] (state-modify (partial sett l b)))
(def += (?= +%))
(def -= (?= -%))
(def *= (?= *%))
(def div= (?= div%))
(def quot= (?= quot%))


;; aliases

(def § hcomp)
(def *> hcomp)
(def *- attrs)
(def each traverse)