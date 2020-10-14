(ns nsufit.task2)

(defn trapezoids-sum
  "Calculates integral(f) from 0 to xcur with step h."
  [f, h, xcur]
  (let [
        square (fn [x0, x1] (*(/(+ (f x0) (f x1)) 2) h)),
        curried (partial trapezoids-sum f h),
        next (- xcur h)
        ]
    (if (<= xcur h)
        (* (/ (square xcur 0) h) xcur)
      (+ (square xcur next) (curried next))
      )
    )
  )

(def mem-trapezoids-sum
  "Memoized version of trapezoids-sum"
  (memoize trapezoids-sum)
  )


(defn get-integral
  [f]
  "Integrate operator - takes f and returns function which
  will calculate integral from 0 to it argument"
  (let [h 0.01]
    (fn [x]
      (trapezoids-sum f h x))
    )
  )

(defn get-integral-mem
  "Memoized version of get-integral"
  [f]
  (let [h 0.01]
    (fn [x] (mem-trapezoids-sum f h x))
    )
  )

(defn calculus
  "Graphics draw emulation"
  [f, integral-getter, n]
  (let [integral (integral-getter f)]
    (doall (map integral (range 1 n))))
  )


; Benchmarks for memoize (h=1)
;(def a (time (calculus identity get-integral 3000)))
;"Elapsed time: 4831.1479 msecs"
;(def a (time (calculus (fn [x] (* x x)) get-integral 3000)))
;"Elapsed time: 4317.1479 msecs"
;(def a (time (calculus (fn [x] (* x x)) get-integral 3000)))
;"Elapsed time: 4320.7465 msecs"

;(def a (time (calculus (fn [x] (* x x)) get-integral-mem 3000)))
;"Elapsed time: 3306.1826 msecs"
;(def a (time (calculus (fn [x] (* x x)) get-integral-mem 3000)))
;"Elapsed time: 4.3511 msecs"
;(def a (time (calculus (fn [x] (* x x)) get-integral-mem 3000)))
;"Elapsed time: 4.5769 msecs"
;(def a (time (calculus (fn [x] (* x x)) get-integral-mem 4000)))
;"Elapsed time: 3514.1047 msecs"
;(def a (time (calculus (fn [x] (* x x)) get-integral-mem 4000)))
;"Elapsed time: 3.84 msecs"

;(def a (time (calculus identity get-integral 4000)))
;"Elapsed time: 7692.9478 msecs"
;(def a (time (calculus identity get-integral 4000)))
;"Elapsed time: 7828.0422 msecs"


(defn partials
  ([f, h, x] (partials f h x x 0))
  ([f, h, x, xcur, prevsquares]
   (let [fcur (f xcur),
         hnext (- xcur h),
         squares (+ fcur prevsquares (f hnext))
         ]
     (if (> xcur h)
       (lazy-seq (cons squares (partials f h x  hnext squares)))
     (list (* (/ (+ prevsquares fcur) 2) h))
       )
     )
   )
  )

(defn get-integral-partial
  [f]
  (fn [x]
    (let [h 0.01]
      (first (take-last 1 (partials f h x)))
      )
    )
  )

; Benchmarks for get-integral-partial (h=1)
;(time (calculus identity get-integral-partial 3000))
;"Elapsed time: 887.8372 msecs"
;(time (calculus identity get-integral-partial 3000))
;"Elapsed time: 818.718 msecs"
;(time (calculus identity get-integral-partial 4000))
;"Elapsed time: 1382.0636 msecs"
;(time (calculus identity get-integral-partial 4000))
;"Elapsed time: 1359.1425 msecs"