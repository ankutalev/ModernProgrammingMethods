(ns nsufit.task1)


;1.3
(defn myMap
  [f, xs]
  (reduce (fn [x, y] (conj x (f y))) (empty xs) xs)
  )

(defn myFilter
  [f, xs]
  (reduce (fn [x, y] (if (f y) (conj x y) x)) (empty xs) xs)
  )

;1.4
(defn my-mapcat
  [f, arg]
  (reduce (fn [acc, c] (concat acc (f c))) (empty arg) arg)
  )

(defn append-char-no-duplicates
  [c, xs]
  (reduce (fn [acc, current_word] (if (.startsWith current_word c) acc (cons (str c current_word) acc))) (empty xs) xs)
  )

(defn join-alphabet
  [xs, alphabet]
  (my-mapcat (fn [c] (append-char-no-duplicates c xs)) alphabet)
  )

(defn task-14
  ([arg, n] (task-14 arg n arg))
  ([arg, n, alphabet] (if (== n 1) arg (task-14 (join-alphabet arg alphabet) (- n 1) alphabet)))
  )


;1.1
(defn list-concat
  ([xs, cur_alphabet, alphabet]
   (let [rest_words (rest xs), rest_symbols (rest cur_alphabet), current_symb (first xs), current_word (first cur_alphabet)]
       (if (empty? xs)
         (empty xs)
       (if (empty? cur_alphabet)
         (list-concat rest_words alphabet alphabet)
       (if (.startsWith current_word current_symb)
         (list-concat xs rest_symbols alphabet)
           (cons (str current_symb current_word) (list-concat xs rest_symbols alphabet)))))
     )
   )
  ([xs ys] (list-concat xs ys ys))
  )

(defn task-11
  ([alph n] (task-11 alph n alph))
  ([xs n res] (if (== n 1) res (task-11 xs (- n 1) (list-concat xs res))))
  )

;1.2
(defn list-concat12
  ([xs, ys, acc, res]
   (let [rxs (rest xs), rys (rest ys), fxs (first xs), fys (first ys)]
     (if (empty? xs) res (if (empty? ys)
                            (recur rxs acc acc res)
                            (if (.startsWith fys fxs)
                                (recur xs rys acc res)
                              (recur xs rys acc (cons (str fxs fys) res))))
     ))
   )
  ([xs ys] (list-concat12 xs ys ys ()))
  )

(defn task-12
  ([xs n] (task-12 xs n xs))
  ([xs n res] (if (== n 1) res (recur xs (- n 1) (list-concat12 xs res))))
  )

