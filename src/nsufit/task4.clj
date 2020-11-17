(ns nsufit.task4)

(defn variable
  [name]
  {:pre [(keyword? name)]}
  (list ::var name)
  )

(defn constant
  [name]
  ;{:pre [(keyword? name)]}
  (list ::const name)
  )

(defn args
  [expr]
  (rest expr)
  )

(declare to-dnf)

(defn conjunction
  [expr & rest]
  (cons ::conjunction (cons expr rest))
  )

(defn disjunction
  [expr & rest]
  (cons ::disjunction (cons expr rest))
  )

(defn negate
  [expr & rest]
  (cons ::negate (cons expr rest))
  )


(defn negate-rule
  [expr]
  (let [
        expr-type (first expr)
        expr-body (rest expr)
        root-cases {::const (fn [expr] (constant ({::false ::true, ::true ::false } (first expr)))),
                    ::negate (fn [expr] (to-dnf (first expr))),
                    ::var (fn [var-name] (negate (variable (first var-name)))),
                    ::conjunction (fn [expr] (to-dnf (disjunction (map negate-rule expr)))),
                    ::disjunction (fn [expr] (to-dnf (conjunction (map negate-rule expr)))),
                    }
        rule-to-apply (or (root-cases expr-type) (fn [nu] (to-dnf (negate-rule (to-dnf expr)))))
        ]
    (rule-to-apply expr-body)
    )
  )

;todo not read yet
(defn disjunction-rule
  [expr]
  (let [
        first-type (first (first expr))
        first-body (rest (first expr))
        second-part (first (rest expr))
        root-cases {::const (fn [ct] (disjunction (constant (first ct)) (to-dnf second-part))), ; todo optimize 1 v A
                    ::var (fn [var-name] (disjunction (variable (first var-name)) (to-dnf second-part))),
                    ::negate      (fn [expr] (disjunction (to-dnf (negate (first expr))) (to-dnf second-part))),
                    ::conjunction (fn [expr] (disjunction (to-dnf (conjunction (first expr))) (to-dnf second-part)))
                    ::disjunction (fn [expr] (disjunction (to-dnf (disjunction (first expr))) (to-dnf second-part))),
                    }
        rule-to-apply (or (root-cases first-type) (fn [nu] (disjunction-rule (to-dnf expr))))
        ]
    (rule-to-apply first-body)
    )
  )

;(A v B) ^ C
;todo not read yet
(defn conjunction-rule
  [expr]
  (let [
        first-type (first (first expr))
        first-body (rest (first expr))
        second-part (first (rest expr))
        root-cases {::const (fn [ct] (conjunction (constant (first ct)) (to-dnf second-part))), ; todo optimize 1 v A
                    ::var (fn [var-name] (conjunction (variable (first var-name)) (to-dnf second-part))),
                    ::negate      (fn [expr] (conjunction (to-dnf (negate (first expr))) (to-dnf second-part))),
                    ::conjunction (fn [expr] (disjunction (to-dnf (conjunction (first expr))) (to-dnf second-part)))
                    ::disjunction (fn [expr] (disjunction (to-dnf (disjunction (first expr))) (to-dnf second-part))),
                    }
        rule-to-apply (or (root-cases first-type) (fn [nu] (disjunction-rule (to-dnf expr))))
        ]
    (rule-to-apply first-body)
    )
  )

(defn to-dnf
  [expr]
  (let [rules {::negate (fn [expr] (negate-rule (first expr)))
               ::conjunction conjunction-rule,
               ::disjunction disjunction-rule,
               ::var (fn [expr] (variable (first expr)))
               ::const (fn [expr] (constant (first expr)))
               }
        expr-type (first expr)
        expr-body (rest expr)
        ]
    ((rules expr-type) expr-body)
    ))


