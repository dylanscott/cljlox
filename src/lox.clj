(ns lox
  (:require [lox.token :refer [tokenize]]
            [lox.parse :refer [parse]]
            [clojure.pprint]
            [clojure.tools.namespace.repl :refer [refresh]]))

(comment (refresh))

(def pprint (bound-fn* clojure.pprint/pprint))
(comment
  (add-tap pprint))
(comment
  (remove-tap pprint))

(defn parse-source [source]
  (let [tokens (tokenize source)]
    (tap> tokens)
    (let [parsed (parse tokens)]
      (tap> parsed)
      parsed)))

(comment
  (parse-source "(1 -2) / 3"))
(comment
  (parse-source "((1)"))
