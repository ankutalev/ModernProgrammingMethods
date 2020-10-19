(ns nsufit.task22)


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