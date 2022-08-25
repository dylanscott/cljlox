(ns lox
  (:require [lox.tokenize :refer [tokenize]]
            [lox.parse :refer [parse]]
            [lox.evaluate :refer [evaluate]]
            [clojure.pprint]
            [clojure.tools.namespace.repl :refer [refresh]]))

(comment (refresh))

(def pprint (bound-fn* clojure.pprint/pprint))
(add-tap pprint)
(comment
  (remove-tap pprint))

(defn run [source]
  (let [tokens (tokenize source)]
    (tap> tokens)
    (let [parsed (parse tokens)]
      (tap> parsed)
      (evaluate parsed))))

(comment
  (run "(1 -2) / 3"))
(comment
  (run "2 >= 2"))
