(ns tech.jna-test
  (:require [tech.jna :as jna]
            [clojure.test :refer :all]))


(deftest string-test
  (testing "Conversion to/from string is simple"
    (let [src-string "lorum ipsum"
          test-ptr (jna/string->ptr src-string)
          result (jna/variable-byte-ptr->string test-ptr)]
      (is (= src-string result)))))
