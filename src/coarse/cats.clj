(ns coarse.cats)

(defn pure [x]
  [:pure x x])

(defn pure? [v]
  (and (vector? v)
       (> (count v) 0)
       (= (first v) :pure)))


(defn classify-type [x]
  (if-let [t (:type x)]
    t
    (cond
      (fn? x) :fn
      (pure? x) :pure
      (coll? x) :coll)))

(defn mk-identity [x]
  {:type :identity :value x})

(defn mk-const [x]
  {:type :const :value x})

(defn mk-first [x]
  {:type :first :value x})

(defn mk-tuple [a b]
  {:type :tuple :value [a b]})

(defn just [x]
  {:value x :type :maybe})

(def nothing {:value nil :type :maybe})

(defn nothing? [x]
  (nil? (:value x)))

(defn spoon [x]
  (if (nil? x)
    nothing
    (just x)))

(defn cons'
  ([x xs] (cons x (if (pure? xs) (second xs) xs)))
  ([x] (partial cons' x)))

(defn lift-pure [t [_ v o]]
  (if (= t :const)
    {:value [] :type t}
    {:value v :type t}))

(defmulti fmap (fn [x y] (classify-type y)))

(defmulti app (fn [x y]
                (if (pure? x)
                  (classify-type y)
                  (classify-type x))))

(defn traverse
  ([f ta]
   (if (empty? ta)
     (pure [])
     (let [x (first ta)
           xs (rest ta)
           fm' (fmap cons' (f x))
           lt' (traverse f xs)
           fm (if (and (pure? fm') (:type lt'))
                (lift-pure (:type lt') fm')
                fm')
           lt (if (and (pure? lt') (:type fm'))
                (lift-pure (:type fm') lt')
                lt')]
       (app fm lt))))
  ([f]
   (partial traverse f)))

(def mempty :mempty)

(defmulti mappend (fn [x y]
                    (if (= x mempty)
                      (classify-type y)
                      (classify-type x))))


(defmethod mappend :coll
  [a b]
  (cond
    (= a mempty) b
    (= b mempty) a
    :else (concat a b)))

(defn implicit-lift-first [a]
  (if (= a mempty) (mk-first nothing) a))

(defmethod mappend :first
  [a b]
  (cond
    (= a mempty) (implicit-lift-first b)
    :else (implicit-lift-first a)))

(defn rest' [a]
  (if (pure? a) [] (rest a)))

(defmethod app :maybe
  [f m']
  (let [m (if (pure? m') (just (second m')) m')]
    (if (or (nothing? f) (nothing? m))
      nothing
      (just ((:value f) (:value m))))))

(defmethod app :coll
  [fs l']
  (let [l (if (pure? l') [(second l')] l')]
    (if (empty? l)
      []
      (apply concat
             (for [f fs]
               (mapv f l))))))

(defmethod app :identity
  [a' b']
  (let [a (if (pure? a') (mk-identity (second a')) a')
        b (if (pure? b') (mk-identity (second b')) b')]
    (mk-identity ((:value a) (:value b)))))

(defn lift-list [x]
  (if (coll? x)
    x
    [x]))

(defmethod app :pure
  [[_ a x] [_ b y]]
  (let [v1 (if (coll? a) (mk-identity a) (mk-const []))
        v2 (if (coll? a) (mk-identity b) (mk-const []))]
    (if (coll? a)
      (mk-identity [])
      (mk-const []))))

(defmethod app :const
  [f v']
  (let [v (if (pure? v') (mk-const mempty) v')]
    (mk-const (mappend (:value f) (:value v)))))

(defmethod fmap :pure
  [f [_ v o]]
  [:pure (f v) o])

(defmethod fmap :identity
  [f {:keys [value]}]
  (mk-identity (f value)))

(defmethod fmap :coll
  [fv coll]
  (mapv fv coll))

(defmethod fmap :maybe
  [f m]
  (if (nothing? m)
    m
    (just (f (:value m)))))

(defmethod fmap :const
  [f v]
  v)

(defmethod fmap :tuple
  [f {:keys [value]}]
  (let [[a b] value]
    (mk-tuple a (f b))))
