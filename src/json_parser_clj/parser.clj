(ns json-parser-clj.parser
  (:use [json-parser-clj.token]))

; arr => [:j-array, [json,...] ]
; obj => [:j-object, [[str, json],...] ]
; bool => [:j-bool, v]
; number => [:j-number, v]
; string => [:j-string, v]
; null => [:j-null]

(declare p-string p-number p-bool p-null p-array p-object p-json)

(defn- match [type tokens]
  (if (look-ahead-type? type tokens)
    (rest tokens)
    (throw (Exception. (str "match token error, need " type ", get" (str (first tokens)))))))

(defn- p-basic [type tokens]
  (if (look-ahead-type? type tokens)
    [[(keyword (clojure.string/lower-case (str "j-" (clojure.core/name type)))), (get-token-val (first tokens))] (rest tokens)]
    (throw (Exception. (str "p-basic error, need " type ", get" (str (first tokens)))))))

(defn- p-string [tokens] (p-basic :STRING tokens))
(defn- p-number [tokens] (p-basic :NUMBER tokens))
(defn- p-bool [tokens] (p-basic :BOOL tokens))
(defn- p-null [tokens] (p-basic :NULL tokens))

(defn- p-array [tokens]
  (let [tokens (match :L-SQUARE-BRACKET tokens)]
    (loop [tokens tokens
           elements []]
      (if (look-ahead-type? :R-SQUARE-BRACKET tokens)
        (let [tokens (match :R-SQUARE-BRACKET tokens)]
          [[:j-array elements] tokens])
        ;else
        (let [[element tokens] (p-json tokens)
              tokens (if (and (look-ahead-type? :COMMA tokens) (not (look-ahead-ahead-type? :R-SQUARE-BRACKET tokens))) (match :COMMA tokens) tokens)]
          (recur tokens (conj elements element)))))))

(defn- p-object [tokens]
  (let [tokens (match :L-CURLY-BRACKET tokens)]
    (loop [tokens tokens
           kvs []]
      (if (not (look-ahead-type? :STRING tokens))
        (let [tokens (match :R-CURLY-BRACKET tokens)] [[:j-object kvs] tokens])
        ;else
        (let [[key tokens] (p-string tokens)
              [value tokens] (->> (match :COLON tokens)
                                  (p-json))
              tokens (if (and (look-ahead-type? :COMMA tokens) (not (look-ahead-ahead-type? :R-CURLY-BRACKET tokens))) (match :COMMA tokens) tokens)]
          (recur tokens (conj kvs [(second key) value])))))))

(defn- p-json [tokens]
  (let [next-token (first tokens)]
    (case (get-token-type next-token)
      :L-SQUARE-BRACKET (p-array tokens)
      :L-CURLY-BRACKET (p-object tokens)
      :NUMBER (p-number tokens)
      :STRING (p-string tokens)
      :BOOL (p-bool tokens)
      :NULL (p-null tokens)
      ;(throw (Exception. (str "match next token error, got " next-token)))
      )))

(defn parse [tokens]
  (let [[ast rest-tokens] (p-json tokens)]
    (if (empty? rest-tokens)
      ast
      (throw (Exception. (str "extra tokens " rest-tokens))))))