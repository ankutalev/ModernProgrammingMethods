(ns nsufit.task32)

(defn my-partition
  [n, collection]
  (map first (iterate (fn [[first-n tail]] [(take n tail) (drop n tail)]) [(take n collection) (drop n collection)] )
  )
  )

(def naturals
  (lazy-seq (cons 1 (map inc naturals)))
  )

(defn my-filter
  [f collection]
    (if (empty? collection)
      ()
      (let [h (first collection) tail (lazy-seq (rest collection))]
         (lazy-seq (if (f h)
                  (cons h (my-filter f tail))
                  (my-filter f tail)))

    ))
  )

(defn my-parallel-lazy-filter
  [block-size, f, coll]
  (mapcat not-empty (map deref (map (fn [block] (future (my-filter f block))) (my-partition block-size coll)
  )))
  )

