(ns tech.v3.jna-test
  (:require [tech.v3.jna :as jna]
            [clojure.test :refer :all])
  (:import [com.sun.jna Pointer]))


(deftest string-test
  (testing "Conversion to/from string is simple"
    (let [src-string "lorum ipsum"
          test-ptr (jna/string->ptr src-string)
          result (jna/variable-byte-ptr->string test-ptr)]
      (is (= src-string result)))))


(deftest as-ptr-nil-test
  (testing "as-ptr on nil returns nil."
    (is (= nil (jna/as-ptr nil)))))


(jna/def-jna-fn (jna/c-library-name) strcpy
  "Copy a (hopefully) null terminated string into a pointer.  This is a horrible idea."
  Pointer
  [dest jna/ensure-ptr]
  [src jna/ensure-ptr])


(deftest strcpy-test
  (let [src-str "dog jumped over moon"
        strlen (inc (count src-str))
        src (jna/string->ptr src-str)
        dst (jna/malloc strlen)]
    (strcpy dst src)
    (is (= src-str (jna/variable-byte-ptr->string dst)))))

(jna/def-jna-fn "doesnt-exist" phantom
 "Verify that a unloadable library is handled gracefully"
 Pointer
 [dest jna/ensure-ptr]
 [src jna/ensure-ptr])

(deftest phantom-test
  (let [src-str "dog jumped over moon"
        strlen (inc (count src-str))
        src (jna/string->ptr src-str)
        dst (jna/malloc strlen)]
    (is (thrown? clojure.lang.ExceptionInfo (phantom dst src)))))
