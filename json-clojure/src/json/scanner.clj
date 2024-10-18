(ns json.scanner
  (:require [clojure.string :as str]))

(defrecord Token [type value])

(declare scan continue-scan)

(defn illegal-state [& messages]
  (throw (IllegalStateException. (str messages))))

(def white? ^[char] Character/isWhitespace)
(def digit? ^[char] Character/isDigit)
(def alpha? ^[char] Character/isLetter)

(defn scan-string [text tokens]
  (let [[s [end & next]] (split-with #(not= \" %) text)]
    (if (not= end \")
      (illegal-state "Unterminated string")
      (continue-scan next tokens :string (str/join s)))))

(defn scan-num [text tokens]
  (let [[s1 next1] (split-with digit? text)
        [s2 next2] (if (= (first next1) \.)
                     (split-with digit? (next next1))
                     [nil next1])]
    (->>
      (concat s1 "." s2)
      (str/join)
      (parse-double)
      (continue-scan next2 tokens :number))))

(defn scan-alpha [text tokens]
  (let [[s next] (split-with alpha? text)]
    (case (str/join s)
      "true" (continue-scan next tokens :boolean true)
      "false" (continue-scan next tokens :boolean false)
      "null" (continue-scan next tokens :null)
      (illegal-state "Unexpected token " s))))

(defn continue-scan
  ([next tokens type]
   (continue-scan next tokens type nil))
  ([next tokens type value]
   (scan next (conj tokens (->Token type value)))))

(defn scan
  ([text] (scan text []))
  ([text tokens]
   (let [c (first text)]
     (case c
       nil tokens
       \{ (continue-scan (next text) tokens :open-brace)
       \} (continue-scan (next text) tokens :close-brace)
       \[ (continue-scan (next text) tokens :open-bracket)
       \] (continue-scan (next text) tokens :close-bracket)
       \, (continue-scan (next text) tokens :comma)
       \: (continue-scan (next text) tokens :colon)
       \" (scan-string (next text) tokens)
       \. (scan-num (cons \0 text) tokens)
       (cond
         (white? c) (scan (next text) tokens)
         (digit? c) (scan-num text tokens)
         (alpha? c) (scan-alpha text tokens)
         :else (illegal-state "Unexpected character " c))
       ))))
