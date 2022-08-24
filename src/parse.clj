(ns lox.parse)

;; grammar:

;; expression -> equality
;; equality   -> comparison ( ( "!=" | "==" ) comparison )*
;; comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )*
;; term       -> factor ( ( "-" | "+" ) factor )*
;; factor     -> unary ( ( "/" | "*" ) unary )*
;; unary      -> ( ( "!" | "-" ) unary ) | primary
;; primary    -> NUMBER | STRING | "true" | "false" | "nil" | "("  expression ")"

(defn- match? [tokens types]
  (let [next-token (first tokens)]
    (and (not (nil? next-token))
         (some (partial = (next-token :type)) types))))

(defn- parse-error [message token]
  (ex-info message { :token token }))

(declare equality)

(defn expression [tokens]
  (-> tokens equality :expr))

(defn parse [tokens]
  (try
    (expression tokens)
    (catch Exception e
      (print (ex-message e) (ex-data e))
      nil)))

(defn- primary [tokens]
  (let [return (fn [value] { :expr { :type :literal :value value } :rest (rest tokens) })
        token (first tokens)]
    (case (-> token :type)
      :false (return false)
      :true (return true)
      :nil (return nil)
      :number (return (token :literal))
      :string (return (token :literal))
      :left-paren (let [{ expr :expr remaining-tokens :rest } (equality (rest tokens))
                        next-token (first remaining-tokens)]
                    (if (= :right-paren (-> next-token :type))
                      { :expr { :type :grouping :expr expr } :rest (rest remaining-tokens) }
                      (throw (parse-error "Expect ')' after expression." next-token)))))))

(defn- unary [tokens]
  (if (match? tokens [:bang :minus])
    (let [operator (-> tokens first :type)
          right (-> tokens rest unary)
          un-expr { :type :unary :operator operator :right (right :expr) }]
      (assoc right :expr un-expr))
    (primary tokens)))

(defn- parse-binary [tokens next-precedence types]
  (loop [{ expr :expr remaining-tokens :rest } (next-precedence tokens)]
    (if (match? remaining-tokens types)
      (let [operator (-> remaining-tokens first :type)
            right (-> remaining-tokens rest next-precedence)
            bin-expr { :type :binary :operator operator :left expr :right (right :expr) }]
        (recur (assoc right :expr bin-expr)))
      { :expr expr :rest remaining-tokens })))

(defn- factor [tokens]
  (parse-binary tokens unary [:slash :star]))

(defn- term [tokens]
  (parse-binary tokens factor [:minus :plus]))

(defn- comparison [tokens]
  (parse-binary tokens term [:greater :greater_equal :less :less_equal]))

(defn- equality [tokens]
  (parse-binary tokens comparison [:bang_equal :equal_equal]))
