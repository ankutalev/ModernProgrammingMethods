(ns nsufit.task31)

(defn my-partition
  [n, collection]
  (if (empty? collection)
    ()
  (let [[part rest] (split-at n collection)]
     (conj (my-partition n rest)  part)
   ))
  )

(defn my-filter
  [f collection]
    (let [h (first collection)]
       (lazy-seq (if (f h) (conj (my-filter f (rest collection)) h) (my-filter f (rest collection))))
    )
  )

(defn my-parallel-filter
  [block-size, f, coll]
    (mapcat deref (doall (map (fn [block] (future (filter f block))) (my-partition block-size coll))
  ))
  )

