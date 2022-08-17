(ns json-parser-clj.parser
  (:use [json-parser-clj.token]))

; arr => ['array', [json,...] ]
; obj => ['object', [[str, json],...] ]
; bool => ['bool', v]
; number => ['number', v]
; string => ['string', v]
; null => ['null']

(declare p-string p-number p-bool p-null p-array p-object p-json)

(defn- match [type tokens]
  (if (check-token type (first tokens))
    (rest tokens)
    (throw (Exception. (str "match token error, need " type ", get" (str (first tokens)))))
    ))

(defn- p-basic [type tokens]
  (if (check-token type (first tokens))
    [[(clojure.string/lower-case (clojure.core/name type)), (get-token-val (first tokens))] (rest tokens)]
    (throw (Exception. (str "p-basic error, need " type ", get" (str (first tokens)))))))

(defn- p-string [tokens] (p-basic :STRING tokens))
(defn- p-number [tokens] (p-basic :NUMBER tokens))
(defn- p-bool [tokens] (p-basic :BOOL tokens))
(defn- p-null [tokens] (p-basic :NULL tokens))

(defn- p-array [tokens]
  (let [tokens (match :L-SQUARE-BRACKET tokens)]
    (if (check-token :R-SQUARE-BRACKET (first tokens))      ;空数组
      [["array" []] (match :R-SQUARE-BRACKET tokens)]
      ;else
      (let [[ele rest-tokens] (p-json tokens)]              ;至少有一个元素
        (loop [tokens rest-tokens
               elements [ele]]
          (if (check-token :COMMA (first tokens))
            (let [rest-tokens (match :COMMA tokens)
                  [next-element rest-tokens] (p-json rest-tokens)]
              (recur rest-tokens (conj elements next-element)))
            ;else
            (let [rest-tokens (match :R-SQUARE-BRACKET tokens)]
              [["array" elements] rest-tokens])))
        )
      ))
  )

(defn- p-object [tokens]
  (let [tokens (match :L-CURLY-BRACKET tokens)]
    (if (check-token :R-CURLY-BRACKET (first tokens))       ;空对象
      [["object" []] (match :R-CURLY-BRACKET tokens)]
      ;else
      (let [[[_ key] tokens] (p-string tokens)
            tokens (match :COLON tokens)
            [value tokens] (p-json tokens)]
        (loop [tokens tokens
               kvs [[key value]]]
          (if (check-token :COMMA (first tokens))
            (let [rest-tokens (match :COMMA tokens)
                  [[_ next-key] rest-tokens] (p-string rest-tokens)
                  rest-tokens (match :COLON rest-tokens)
                  [next-val rest-tokens] (p-json rest-tokens)
                  ]
              (recur rest-tokens (conj kvs [next-key next-val])))
            ;else
            (let [rest-tokens (match :R-CURLY-BRACKET tokens)]
              [["object" kvs] rest-tokens])))
        )
      )))

(defn- p-json [tokens]
  (let [next-token (first tokens)]
    (case (get-token-type next-token)
      :L-SQUARE-BRACKET (p-array tokens)
      :L-CURLY-BRACKET (p-object tokens)
      :NUMBER (p-number tokens)
      :STRING (p-string tokens)
      :BOOL (p-bool tokens)
      :NULL (p-null tokens)
      )
    )
  )

(defn parse [tokens]
  (first (p-json tokens)))

(defn json-minify [ast]
  (case (first ast)
    "number" (second ast)
    "string" (str \" (second ast) \")
    "bool" (second ast)
    "null" "null"
    "array" (let [elements (second ast)]
              (str \[ (clojure.string/join "," (map json-minify elements)) \]))
    "object" (let [kvs (second ast)]
               (str \{ (clojure.string/join "," (map (fn [[k v]] (str \" k \" \: (json-minify v))) kvs)) \}))))

(defn json-stringify [ast indent-width]
  (let [gen-ident (fn [width depth] (apply str (repeat (* depth width) " ")))
        helper (fn helper [depth force-indent? ast]
                 (cond->> (case (first ast)
                            "number" (str (second ast))
                            "string" (str \" (second ast) \")
                            "bool" (second ast)
                            "null" "null"
                            "array" (let [elements (second ast)]
                                      (if (empty? elements)
                                        "[]"
                                        (str \[ "\n" (clojure.string/join ",\n" (map (partial helper (inc depth) true) elements)) "\n" (gen-ident indent-width depth) \])))
                            "object" (let [kvs (second ast)]
                                       (if (empty? kvs)
                                         "{}"
                                         (str \{ "\n"
                                              (clojure.string/join ",\n" (map (fn [[k v]] (str (gen-ident indent-width (inc depth)) \" k \" \: " " ((partial helper (inc depth) false) v))) kvs))
                                              "\n" (gen-ident indent-width depth) \}))))
                          force-indent? (str (gen-ident indent-width depth)))
                 )]
    (helper 0 false ast)))