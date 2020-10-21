(ns nsufit.task32)

(defn my-lazy-partition
  [n, collection]
  (map first (iterate (fn [[first-n tail]] [(take n tail) (drop n tail)]) [(take n collection) (drop n collection)] )
  )
  )

(def naturals
  (lazy-seq (cons 1 (map inc naturals)))
  )

(defn my-lazy-filter
  [f collection]
    (if (empty? collection)
      ()
      (let [h (first collection) tail (lazy-seq (rest collection))]
         (lazy-seq (if (f h)
                     (cons h (my-lazy-filter f tail))
                     (my-lazy-filter f tail)))

    ))
  )

(defn my-parallel-lazy-filter
  [block-size, f, coll]
  (mapcat not-empty (map deref (map (fn [block] (future (my-lazy-filter f block))) (my-lazy-partition block-size coll))))
  )

