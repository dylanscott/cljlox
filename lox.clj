(ns lox
  (:require [lox.token :refer [tokenize]]
            [lox.parse :refer [parse]]
            [clojure.pprint]))

(def pprint (bound-fn* clojure.pprint/pprint))
(comment
  (add-tap pprint))
(comment
  (remove-tap pprint))

(defn parse-source [source]
  (let [tokens (tokenize source)]
    (tap> tokens)
    (let [parsed (parse tokens)]
      (tap> parsed))))

(comment
  (parse-source "(1 -2) / 3"))
