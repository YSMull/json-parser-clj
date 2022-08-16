(ns json-parser-clj.token)

; Token 抽象，可替换，但 type 必须是固定的几个 keyword

;(defn make-token
;  ([type] {:type type})
;  ([type value] {:type type :value value}))

(defrecord Token [type value]
  Object
  (toString [_]
    (if value (str "(" type ", " value ")") (str "(" type ")"))
    ))

(defn make-token
  ([type] (Token. type nil))
  ([type value] (Token. type value)))

(defn check-token [type token] (= type (:type token)))

(defn get-token-type [token] (:type token))

(defn get-token-val [token] (:value token))