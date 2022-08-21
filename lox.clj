(ns lox)

(def keywords
  #{"and"
    "class"
    "else"
    "false"
    "for"
    "fun"
    "if"
    "nil"
    "or"
    "print"
    "return"
    "super"
    "this"
    "true"
    "var"
    "while"})

;; token:
;; { :type :offset :lexeme? }
(defn- next-token [source start]
  (if (>= start (count source))
    nil
    (let [current-char (.charAt source start)
          match (fn [char]
                  (and
                    (< (+ start 1) (count source))
                    (= char (.charAt source (+ start 1)))))
          return (fn [token & [from]]
                   (lazy-seq
                     (cons (assoc token :offset start)
                           (next-token source (or from (+ start 1))))))]
      (case current-char
        \( (return { :type :left_paren })
        \) (return { :type :right_paren })
        \{ (return { :type :left_brace })
        \} (return { :type :right_brace })
        \, (return { :type :comma })
        \. (return { :type :dot })
        \- (return { :type :minus })
        \+ (return { :type :plus })
        \; (return { :type :semicolon })
        \* (return { :type :star })
        \! (if (match \=)
             (return { :type :bang_equal } (+ start 2))
             (return { :type :bang }))
        \= (if (match \=)
             (return { :type :equal_equal } (+ start 2))
             (return { :type :equal }))
        \< (if (match \=)
             (return { :type :less_equal } (+ start 2))
             (return { :type :less }))
        \> (if (match \=)
             (return { :type :greater_equal } (+ start 2))
             (return { :type :greater }))
        \\ (if (match \\)
             (let [eol (.indexOf source (int \newline) start)]
               (if (= eol -1)
                 nil
                 #(next-token source (+ eol 1))))
             (return { :type :slash }))
        \space #(next-token source (+ start 1))
        \tab #(next-token source (+ start 1))
        \return #(next-token source (+ start 1))
        \newline #(next-token source (+ start 1))
        ))))

(defn- string [source start]
  (let [closing-quote (.indexOf source (int \") (+ start 1))]
    (if (= closing-quote -1)
      (cons { :type :error :offset start :error "Unterminated string" } nil)
      (let [lexeme (.substring source start (+ 1 closing-quote))
            literal (.substring lexeme 1 (- (count lexeme) 1))]
        (lazy-seq (cons { :type :string :offset start :lexeme lexeme :literal literal }
                        (next-token source (+ closing-quote 1))))))))

(comment
  (string "\"foo\"" 0))
(comment
  (.indexOf "foo" (int \f)))

(defn- is-alpha? [c]
  (or (and (<= 0 (compare c \a)) (>= 0 (compare c \z)))
      (and (<= 0 (compare c \A)) (>= 0 (compare c \Z)))
      (= c \_)))

(defn- is-digit? [c]
  (and (<= 0 (compare c \0)) (>= 0 (compare c \9))))
(defn- is-alphanumeric? [c]
  (or (is-alpha? c) (is-digit? c)))

(comment
  (is-alpha? \a))
(comment
  (is-digit? \0))

(defn- identifier [source start])

(defn- number [source start]
  (let [return (fn [end]
                 (let [lexeme (.substring source start end)
                       value (Double/parseDouble lexeme)]
                   (lazy-seq (cons
                               { :type :string :offset start :lexeme lexeme :literal value }
                               (next-token source end)))))]
    (loop [i start]
      (if (< i (count source))
        (let [c (.charAt source i)]
          (if (is-digit? c)
            (recur (+ 1 i))
            (if (and (= c \.)
                     (< i (- (count source) 1))
                     (is-digit? (.charAt source (+ i 1))))
              (loop [j (+ i 1)]
                (if (< j (count source))
                  (let [after-decimal (.charAt source j)]
                    (if (is-digit? after-decimal)
                      (recur (+ j 1))
                      (return j)))
                  (return j)))
              (return i))))
        (return i)))))

(comment
  (number "12.0" 0)
  (number "12)" 0))

(defn tokenize [source]
  (trampoline next-token source 0))
