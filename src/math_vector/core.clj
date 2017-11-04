(ns math-vector.core)

(derive java.lang.Number ::scalar)
(derive java.lang.Long ::scalar)
(derive java.lang.Double ::scalar)

(derive ::vector ::tensor)
(derive ::matrix ::tensor)
(derive clojure.lang.PersistentVector ::tensor)


(defmulti shape class)
(defmethod shape ::scalar [s] [])
(defmethod shape ::tensor
  [t]
  (if (isa? (class (first t)) ::tensor)
    (let [counts (map count t)]
      (when (> (count (set counts)) 1)
        (throw (ex-info "Malformed tensor" {:tensor t})))
      (into [(count t)] (shape (first t))))
    [(count t)]))

(defn pairwise
  "Apply a function to two vectors pairwise"
  [fn a b]
  (when-not (= (shape a) (shape b))
    (throw (ex-info "Tensors have different shapes but must be uniform for pairwise calculations" {:a a :b b})))
  (if (isa? (class (first a)) ::tensor)
    (mapv #(pairwise fn %1 %2) a b)
    (mapv fn a b)))

(defn unitwise
  "Apply a function to two vectors pairwise"
  [fn a]
  (if (isa? (class (first a)) ::tensor)
    (mapv #(unitwise fn %) a)
    (mapv fn a)))


(defmulti mul
  (fn [a b]
    [(class a) (class b)]
    ))

(defmethod mul
  [::scalar ::scalar]
  [a b]
  (* a b))

(defmethod mul
  [::tensor ::scalar]
  [a b]
  (mapv (partial * b) a))

(defmethod mul
  [::scalar ::tensor]
  [a b]
  (mul b a))

(defmethod mul
  [::tensor ::tensor]
  [a b]
  (pairwise mul a b))


(defmulti add
  (fn [a b]
    [(class a) (class b)]
    ))

(defmethod add
  [::scalar ::scalar]
  [a b]
  (+ a b))

(defmethod add
  [::tensor ::scalar]
  [a b]
  (unitwise (partial add b) a))

(defmethod add
  [::scalar ::tensor]
  [a b]
  (unitwise (partial add a) b))

(defmethod add
  [::tensor ::tensor]
  [a b]
  (pairwise add a b))
