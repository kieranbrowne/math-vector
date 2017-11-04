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



(defmulti mul
  (fn [a b]
    (mapv
     #(case (count (shape %))
        0 ::scalar
        1 ::vector
        2 ::matrix
        ::tensor)
     [a b])))


(mapv
 #(case (count (shape %))
    0 ::scalar
    1 ::vector
    2 ::matrix
    ::tensor)
 [[[1]] [2]])

(defmethod mul
  [::scalar ::scalar]
  [a b]
  (* a b))



(def shape-error
  )

(defmethod mul
  [::tensor ::scalar]
  [a b]
  (mapv (partial * b) a))

(defmethod mul
  [::scalar ::tensor]
  [a b]
  (mul b a))

(defmethod mul
  [::matrix ::vector]
  [a b]
  (when-not (= (second (shape a)) (first (shape b)))
    (throw
     (ex-info
      (str "Incompatible shapes. "
           "Columns of matrix a must equal rows of  vector b. "
           (second (shape a)) " != " (first (shape b)) ".")
      {:a a :b b
       :a_shape (shape a) :b_shape (shape b)})))

  (apply add (mapv #(mul %1 %2) a b)))

(defmethod mul
  [::tensor ::tensor]
  [a b]
  (pairwise mul a b)
  )

(prefer-method mul [::matrix ::vector] [::tensor ::tensor])


(defmulti div
  (fn [a b]
    [(class a) (class b)]
    ))

(defmethod div
  [::scalar ::scalar]
  [a b]
  (/ a b))

(defmethod div
  [::tensor ::scalar]
  [a b]
  (unitwise #(div % b) a))

(defmethod div
  [::scalar ::tensor]
  [a b]
  (unitwise #(div a %) b))

(defmethod div
  [::tensor ::tensor]
  [a b]
  (pairwise div a b))



(defn size
  "Size of a vector in n dimentional space"
  [v]
  (Math/sqrt
   (transduce
    (map #(* % %))
    + v)))

(defn normalize [v]
  (div v (size v)))
