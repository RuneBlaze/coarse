(ns coarse.macros
  (:require [coarse.core :refer
             [over view sett _index]]))

(defmacro +>>
  "zooming macro"
  [lns & forms]
  `(over ~lns (fn [x#]
                (->> x#
                     ~@(butlast forms)))
         ~(last forms)))

(defmacro using
  "let variant, retrieves values using lenses"
  [bindings & forms]
  (letfn [(wr [x obj]
            `(view ~x ~obj))]
    (let [obj (last forms)
          ops (butlast forms)]
      `(let
         ~(apply vector (over (_index odd?) #(wr % obj) bindings))
         (->> ~obj ~@ops)))))