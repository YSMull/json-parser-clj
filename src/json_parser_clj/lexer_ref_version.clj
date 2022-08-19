(ns json-parser-clj.lexer-ref-version
  (:use [json-parser-clj.token]))

(def input (ref nil))
(def pos (ref 0))
(def c (ref nil))

(defn- consume []
  (do
    (alter pos inc)
    (if (>= @pos (count @input))
      (ref-set c :EOF)
      (ref-set c (nth @input @pos)))))

(defn- BOOL []
  (loop [bool-string ""]
    (cond
      (> (count bool-string) 5) (throw (Exception. (str "token BOOL error, pos at: " @pos " " bool-string)))
      (and (= (count bool-string) 4) (= (clojure.string/lower-case bool-string) "true")) (make-token :BOOL "true")
      (and (= (count bool-string) 5) (= (clojure.string/lower-case bool-string) "false")) (make-token :BOOL "false")
      :else (let [c @c]
              (do (consume)
                  (recur (str bool-string c))))
      )
    )
  )

(defn- NULL []
  (loop [null-string ""]
    (cond
      (> (count null-string) 4) (throw (Exception. (str "token NULL error, pos at: " @pos)))
      (and (= (count null-string) 4) (= (clojure.string/lower-case null-string) "null")) (make-token :NULL)
      :else (let [c @c]
              (do (consume)
                  (recur (str null-string c))))
      )
    )
  )

(defn- NUMBER []
  (let [sign (if (= @c \-) (do (consume) -1) 1)
        _ (if-not (Character/isDigit ^char @c)
            (throw (Exception. (str "token NUMBER error, pos at: " @pos))))
        ]
    (loop [number-string ""
           has-dot false]
      (cond
        (Character/isDigit ^char @c) (let [c @c] (do
                                                   (consume)
                                                   (recur (str number-string c) has-dot)))
        (= @c \.) (if has-dot
                    (throw (Exception. (str "token NUMBER error, pos at: " @pos)))
                    (let [c @c] (do
                                  (consume)
                                  (recur (str number-string c) true))))
        :else (make-token :NUMBER (* sign (if has-dot
                                            (Double/parseDouble number-string)
                                            (Long/parseLong number-string)))))
      )
    ))

(defn- STRING []
  (do
    (consume)
    (loop [string ""]
      (cond
        (= \" @c) (do (consume) (make-token :STRING string))
        (= \\ @c) (do (consume) (let [c @c] (do (consume)
                                                (recur (str string \\ c)))))
        (= :EOF c) (throw (Exception. (str "token STRING error, pos at: " @pos)))
        :else (let [c @c] (do (consume)
                              (recur (str string c))))
        )
      )))

(defn- next-token []
  (loop []
    (cond
      (= @c :EOF) nil
      (or (= \space @c) (= \newline @c) (= \tab @c) (= \return @c)) (do (consume) (recur))
      (= @c \{) (do (consume) (make-token :L-CURLY-BRACKET))
      (= @c \}) (do (consume) (make-token :R-CURLY-BRACKET))
      (= @c \[) (do (consume) (make-token :L-SQUARE-BRACKET))
      (= @c \]) (do (consume) (make-token :R-SQUARE-BRACKET))
      (= @c \,) (do (consume) (make-token :COMMA))
      (= @c \:) (do (consume) (make-token :COLON))
      (= @c \n) (NULL)
      (or (= @c \t) (= @c \f)) (BOOL)
      (= @c \") (STRING)
      (or (= @c \-) (Character/isDigit ^char @c)) (NUMBER)
      )))

(defn tokenize [json-string]
  (dosync
    (ref-set input json-string)
    (ref-set pos 0)
    (ref-set c (nth @input @pos))
    (loop [tokens []]
      (let [next-token (next-token)]
        (cond
          (nil? next-token) tokens
          :else (recur (conj tokens next-token))))
      )))

