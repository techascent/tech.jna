(ns tech.jna
  (:import [com.sun.jna Native NativeLibrary Pointer Function]
           [com.sun.jna.ptr PointerByReference]))



(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)



(def load-library
  (memoize
   (fn [libname]
     (NativeLibrary/getInstance libname))))


(def do-find-function
  (memoize
   (fn [fn-name libname]
     (.getFunction ^NativeLibrary (load-library libname) fn-name))))

(defn find-function
  ^Function [fn-name libname]
  (do-find-function fn-name libname))


(defn unsafe-read-byte
  [^Pointer byte-ary ^long idx]
  (.get (.getByteBuffer byte-ary idx 1) 0))


(defn variable-byte-ptr->string
  "Convert a c-string into a string"
  [^Pointer ptr-addr]
  (if (= 0 (Pointer/nativeValue ptr-addr))
    ""
    (String. ^"[B"
             (into-array Byte/TYPE
                         (take-while #(not= % 0)
                                     (map #(unsafe-read-byte
                                            ptr-addr %)
                                          (range)))))))

(defn checknil
  ^Pointer [value]
  (if (instance? Pointer value)
    (checknil (Pointer/nativeValue value))
    (if (= 0 (long value))
      (throw (ex-info "Pointer value is nil"
                      {}))
      (Pointer. value))))


(defn ensure-type
  [item-cls item]
  (when-not (instance? item-cls item)
    (throw (ex-info "Item is not desired type"
                    {:item-cls item-cls
                     :item item})))
  item)


(defn ensure-ptr-ptr
  ^PointerByReference [item]
  (ensure-type PointerByReference item))


(defn ensure-ptr
  ^Pointer [item]
  (ensure-type Pointer item))


(defn to-typed-fn
  ^Function [item] item)


(defmacro def-jna-fn
  "TVM functions are very regular so the mapping to them can exploit this.
Argpair is of type [symbol type-coersion]."
  [libname fn-name docstring rettype & argpairs]
  `(defn ~fn-name
     ~docstring
     ~(mapv first argpairs)
     (let [~'tvm-fn (find-function ~(str fn-name) ~libname)
           ~'fn-args (object-array ~(mapv (fn [[arg-symbol arg-coersion]]
                                            `(~arg-coersion ~arg-symbol))
                                          argpairs))]
       ~(if rettype
          `(.invoke (to-typed-fn ~'tvm-fn) ~rettype ~'fn-args)
          `(.invoke (to-typed-fn ~'tvm-fn) ~'fn-args)))))
