(ns coarse.core-test
  (:require [clojure.test :refer :all]
            [coarse.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(defspec getter-function-disguise-attr
         (prop/for-all
           [v (gen/vector gen/int)]
           (= (first v) (view (*- 0) v))))

(deftest sett-works-with-simple-lenses
  (testing "ix variants"
    (let [v [:a :b :c :d :e]]
      (is (= (assoc v 3 :y)
             (sett _4 :y v)))))
  (testing "attr variants"
    (let [v {:a [:b :c] :h [:d [:e :f]]}]
      (is (= (assoc-in v [:h 1 0] 10)
             (sett (§ :h _2 _1) 10 v)
             (sett (§ :h 1 0) 10 v)
             (over (§ :h 1 0) (constantly 10) v))))))

(deftest small-helpers
  (testing "to works"
    (is
      (= (view (to :x) {:x 100})
         100)))
  (testing "magnify works"
    (is (=
          ((magnify _1 inc) [34 1])
          35))))

(deftest maths-operators
  (testing "all-maths-operators"
    (let [v [0 1 2 3 4 5]]
      (is (= (+% (_index even?) 3 v)
             [3 1 5 3 7 5]))
      (is (= (-% (_taking 2) 10 v)
             [-10 -9 2 3 4 5]))
      (is (= (*% _last -1 v)
             [0 1 2 3 4 -5]))
      (is (= (quot% (_filtering even?) 2 v)
             (div% (_filtering even?) 2 v)
             [0 1 1 3 2 5])))))

(deftest join-simple
  (testing "joining"
    (let [_13 (join _1 _3)
          v [1 2 3]]
      (is (= (view _13 v) [1 3]))
      (is (= (over _13 (partial map inc) v) [2 2 4]))
      (is (= (sett _13 [:a :b] v) [:a 2 :b])))))

(deftest to-list-of-with-traversals
  (testing "to-list-of"
    (let [v [0 3 2 1 5 3 6 7 8 9]]
      (is (= v
             (to-list-of each v)
             (to-list-of (_ranging (range)) v)))
      (is (= (take 5 v) (to-list-of (_taking 5) v)))
      (is (= (concat (map inc (take 5 v))
                     (drop 5 v))
             (over (_taking 5) inc v))))))

(def game-state
  {:player {:pos {:x 0 :y 0} :hp 100 :inventory []}})

(def move-right-then-up
  (lens-do
    (+= (*> :pos :x) 1)
    (-= (*> :pos :y) 1)))

(deftest state-manipulations
  (testing "basic state setters"
    (is (= (exec-state (zoom (*> :player) move-right-then-up) game-state)
         (assoc-in game-state [:player :pos] {:x 1 :y -1}))))
  (testing "basic state getters"
    (is (= (exec-state (lens-do (<= [x y] (join (*> :player :pos :x) (*> :player :pos :y)))
                                (state-put [x y]))
                       game-state)
           [0 0]))))