(ns tech.jna
  (:require [tech.jna.base :as base]
            [tech.datatype.jna :as dtype-jna]
            [tech.datatype :as dtype])
  (:import [com.sun.jna Native NativeLibrary Pointer Function]
           [com.sun.jna.ptr PointerByReference]))



(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)



(defn load-library
  [libname]
  (base/load-library libname))


(defn find-function
  ^Function [fn-name libname]
  (base/find-function fn-name libname))


(defn unsafe-read-byte
  [^Pointer byte-ary ^long idx]
  (base/unsafe-read-byte byte-ary idx))


(defn variable-byte-ptr->string
  "Convert a c-string into a string"
  [^Pointer ptr-addr]
  (base/variable-byte-ptr->string ptr-addr))


(defn string->ptr
  ^Pointer [^String data]
  (let [str-bytes (.getBytes data "ASCII")
        num-bytes (+ (alength str-bytes) 1)
        typed-data (dtype-jna/make-typed-pointer :int8 num-bytes)]
    (dtype/set-constant! typed-data 0 0 (dtype/ecount typed-data))
    (dtype/copy! str-bytes typed-data)
    (dtype-jna/->ptr-backing-store typed-data)))


(defn checknil
  ^Pointer [value]
  (base/checknil value))


(defn ensure-type
  [item-cls item]
  (base/ensure-type item-cls item))


(defn ensure-ptr-ptr
  ^PointerByReference [item]
  (base/ensure-ptr-ptr item))


(defn ensure-ptr
  ^Pointer [item]
  (base/ensure-ptr item))


(defmacro def-jna-fn
  "TVM functions are very regular so the mapping to them can exploit this.
Argpair is of type [symbol type-coersion]."
  [libname fn-name docstring rettype & argpairs]
  `(base/def-jna-fn ~libname ~fn-name ~docstring ~rettype ~@argpairs))
