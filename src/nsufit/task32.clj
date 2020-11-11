(ns nsufit.task32)

(defn my-lazy-partition
  [n, collection]
  (if (empty? collection) () (lazy-seq (cons (take n collection) (my-lazy-partition n (drop n collection)))))
  )

(def naturals
  (lazy-seq (cons 1 (map inc naturals)))
  )

(defn my-pred2
  [x]
  (println "Sleeping 1 sec At thread" (.getName (Thread/currentThread)))
  (Thread/sleep 1000)
  (identity x)
  )

(defn my-parallel-lazy-filter
  [block-size, f, coll]
  (let [workers 30,
        blocks (my-lazy-partition block-size coll)
        jobs (take workers blocks)
        rests (drop (reduce + (map count jobs)) coll)
        result (mapcat deref (doall (map (fn [block] (future (doall (filter f block)))) jobs)))
        ]
    (if (empty? rests) result (lazy-cat result (my-parallel-lazy-filter block-size f rests)))
    )
  )
