(ns json.scanner-test
  (:require [clojure.test :refer [deftest is]]
            [json.scanner :refer [->Token scan]]))

(defn tkn
  ([type] (->Token type nil))
  ([type value] (->Token type value)))

(defn test-scan [expected text]
  (is (= expected (scan text))))

(deftest test-scan-empty
  (let [expected [(tkn :open-brace) (tkn :close-brace)]]
    (test-scan expected "{}")))

(deftest test-scan-string
  (let [expected [(tkn :string "string")]]
    (test-scan expected "\"string\"")))

(deftest test-scan-int
  (let [expected [(tkn :number 123.0)]]
    (test-scan expected "123")))

(deftest test-scan-decimal
  (let [expected [(tkn :number 123.456)]]
    (test-scan expected "123.456")))

(deftest test-scan-headless-decimal
  (let [expected [(tkn :number 0.456)]]
    (test-scan expected ".456")))

(deftest test-scan-true
  (let [expected [(tkn :boolean true)]]
    (test-scan expected "true")))

(deftest test-scan-false
  (let [expected [(tkn :boolean false)]]
    (test-scan expected "false")))

(deftest test-scan-null
  (let [expected [(tkn :null)]]
    (test-scan expected "null")))

(deftest test-scan-tree
  (let [expected
        [(tkn :open-brace)
         (tkn :string "aString")
         (tkn :colon)
         (tkn :string "string1")
         (tkn :comma)
         (tkn :string "aNum")
         (tkn :colon)
         (tkn :number 123.0)
         (tkn :comma)
         (tkn :string "aDecimal")
         (tkn :colon)
         (tkn :number 33.4)
         (tkn :comma)
         (tkn :string "aBool")
         (tkn :colon)
         (tkn :boolean true)
         (tkn :comma)
         (tkn :string "anotherBool")
         (tkn :colon)
         (tkn :boolean false)
         (tkn :comma)
         (tkn :string "aNull")
         (tkn :colon)
         (tkn :null)
         (tkn :close-brace)]
        json "{
          \"aString\": \"string1\",
          \"aNum\": 123,
          \"aDecimal\": 33.4,
          \"aBool\": true,
          \"anotherBool\": false,
          \"aNull\": null
        }"]
    (test-scan expected json)))
