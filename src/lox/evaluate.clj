(ns lox.evaluate)

(def operator-map
  {:bang not
   :minus -'
   :plus +'
   :star *'
   :slash /
   :greater #(boolean (> %1 %2))
   :greater-equal #(boolean (>= %1 %2))
   :less #(boolean (< %1 %2))
   :less-equal #(boolean (<= %1 %2))
   :bang-equal not=
   :equal-equal =
   })

(defn evaluate [expr]
  (case (expr :type)
    :literal (expr :value)
    :grouping (-> expr :expr eval)
    :unary (let [right (-> expr :right eval)
                 op (-> expr :operator operator-map)]
             (op right))
    :binary (let [left (-> expr :left eval)
                  right (-> expr :right eval)
                  op (-> expr :operator operator-map)]
              (op left right))))

(comment
  (eval { :type :literal :value "ok" }))
