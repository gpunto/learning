(ns json.utils)

(defn illegal-state [& messages]
  (throw (IllegalStateException. (apply str messages))))

