(ns nsufit.task21)

(defn trapezoid-square
  [x1 x2 h]
  "Calculate trapzeoid square with sizes x1, x2 and midsegment h"
  (* (/(+ x1 x2) 2) h)
  )

(defn trapezoids-sum
  [f h xcur]
  "Calculate integral of f with trapezoid method with step h from 0 to xcur"
  (let [next (- xcur h)]
     (if (< xcur h)
       0
       (+ (trapezoid-square (f xcur) (f next) h) (trapezoids-sum f h next))
       )
   ))


(def trapezoids-sum-mem
  "Memoized version of trapezoids-sum"
   (memoize
     (fn [f h xcur]
      (let [next (- xcur h)]
        ;debug print to see memoization
        ;(println "trapezoids-sum(" xcur ")")
         (if (< xcur h)
           0
          (+ (trapezoid-square (f xcur) (f next) h) (trapezoids-sum-mem f h next))
          ))
      )
     )
  )


(defn generic-integral
  [f, integrate-func]
  (let [h 0.01]
    (fn [x]
      (let [nearest (* h (Math/floor (/ x h))), restx (- x nearest)]
        (+ (integrate-func f h nearest) (trapezoid-square (f nearest) (f x) restx))
        ))
    )
  )

(defn get-integral
  [f]
  "Integrate operator - takes f and returns function which
  will calculate integral from 0 to it argument"
  (generic-integral f trapezoids-sum)
  )

(defn get-integral-mem
  [f]
  "memoized version of get-integral"
  (generic-integral f trapezoids-sum-mem)
  )

(defn calculus
  "Graphics draw emulation"
  [f, integral-getter, n]
  (let [integral (integral-getter f)]
    (doall (map integral (range 1 n))))
  )
