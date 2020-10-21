(ns nsufit.task22
(:require [nsufit.task21 :refer :all]))

(defn partials
  ([f  h]
     (iterate
       (fn [[curx acc]]
         (let [fcur (f curx), nextx (+ curx h)]
          [nextx (+ acc (trapezoid-square fcur (f nextx) h) )]
         ))
       [0  0])
     )
  )


(defn get-integral-partial
  [f]
  (fn [x]
    (let [h 0.01, hcounts (Math/floor (/ x h)), rest (- x (* h hcounts))]
      (+ (trapezoid-square (f (* h hcounts)) (f x) rest) (second (nth (partials f h) hcounts)))
      )
    )
  )
