(ns json-parser-clj.token)

; Token 抽象，可替换，但 type 必须是固定的几个 keyword
; :L-SQUARE-BRACKET
; :R-SQUARE-BRACKET
; :L-CURLY-BRACKET
; :R-CURLY-BRACKET
; :COMMA
; :COLON
; :STRING
; :NUMBER
; :BOOL
; :NULL

(defn make-token
  ([type] {:type type})
  ([type value] {:type type :value value}))

;(defrecord Token [type value])
;
;(defn make-token
;  ([type] (Token. type nil))
;  ([type value] (Token. type value)))

(defn look-ahead-type? [type tokens] (= type (:type (first tokens))))

(defn get-token-type [token] (:type token))

(defn get-token-val [token] (:value token))