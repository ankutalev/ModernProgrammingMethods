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
  (if (empty? collection)
    ()
    (let [h (first collection) tail (lazy-seq (rest collection))]
      (lazy-seq (if (f h)
                  (cons h (my-filter f tail))
                  (my-filter f tail)))

      ))
  )

(defn my-pred
  [x]
  (println "Sleeping 1 sec At thread" (.getName (Thread/currentThread)))
  (Thread/sleep 1000)
  (identity x)
  )

(defn my-parallel-filter
  [block-size, f, coll]
  (let [blocks (my-partition block-size coll)]
    (mapcat deref (doall (map (fn [block] (future (doall (filter f block)))) blocks))))
  )