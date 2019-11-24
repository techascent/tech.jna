(ns tech.jna
  (:require [tech.jna.base :as base]
            [tech.resource :as resource])
  (:import [com.sun.jna Native NativeLibrary Pointer Function Platform Structure]
           [com.sun.jna.ptr PointerByReference LongByReference IntByReference]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defprotocol PToPtr
  (is-jna-ptr-convertible? [item])
  (->ptr-backing-store [item]
    "Conversion to a jna pointer type that points to the data of the object."))


(extend-type Object
  PToPtr
  (is-jna-ptr-convertible? [item] false))


(defn ptr-convertible?
  [item]
  (is-jna-ptr-convertible? item))


(defn as-ptr
  [item]
  (when (and item (ptr-convertible? item))
    (->ptr-backing-store item)))


(extend-protocol PToPtr
  Pointer
  (is-jna-ptr-convertible? [item] true)
  (->ptr-backing-store [item] item)
  PointerByReference
  (is-jna-ptr-convertible? [item] true)
  (->ptr-backing-store [item] (.getValue ^PointerByReference item))
  Structure
  (is-jna-ptr-convertible? [item] true)
  (->ptr-backing-store [item] (.getPointer item)))


(defn add-library-path
  "Add a search path.  The multimethod (base/find-library pathtype path) is called to
  expand the pathtype, path into one or more actual paths to attempt.  Valid existing
  pathtypes are

:system - no changes, looks in system paths.
:java-library-path - Appends library to all paths in java-library-paths
:resource - Uses jna Native/extractFromResourcePath"
  [libname pathtype path]
  (base/add-library-path libname pathtype path))


(defn clear-library-paths
  "Clear the library search paths for a specific library.
Use with care; the default if non found is:
[[:system libname]
 [:java-library-path libname]]."
  [libname]
  (base/clear-library-paths libname))


(defn library-paths
  "Get the current library search paths for a library."
  [libname]
  (base/library-paths libname))


(defn map-shared-library-name
  "Map a stem to a shared library name in platform specific manner"
  [libname]
  (base/map-shared-library-name libname))


(defn set-loaded-library!
  "Override the search mechanism and set the native library to X."
  [libname native-library]
  (base/set-loaded-library! libname native-library))


(defn load-library
  ^NativeLibrary [libname]
  (base/load-library libname))


(defn find-function
  ^Function [fn-name libname]
  (base/find-function fn-name libname))


(defn malloc-untracked
  "Malloc pointer of Y bytes.  Up to call to call Native/free on result at some point"
  ^Pointer [^long num-bytes]
  (Pointer. (Native/malloc num-bytes)))


(defn malloc
  "Malloc a pointer of Y bytes.  Track using both resource context
  and gc system."
  ^Pointer [^long num-bytes]
  (let [retval (malloc-untracked num-bytes)
        native-value (Pointer/nativeValue retval)]
    (resource/track retval
                    #(Native/free native-value)
                    [:gc :stack])))


(defn unsafe-read-byte
  [^Pointer byte-ary ^long idx]
  (base/unsafe-read-byte byte-ary idx))


(defn variable-byte-ptr->string
  "Convert a c-string into a string"
  ^String [^Pointer ptr-addr]
  (.getString ptr-addr 0 "ASCII"))


(defn char-ptr-ptr->string-vec
  "Decode a char** ptr."
  [^long num-strings ^Pointer char-ptr-ptr]
  (base/char-ptr-ptr->string-vec num-strings char-ptr-ptr))


(defn string->ptr
  ^Pointer [^String data]
  (let [^Pointer retval (malloc (+ 1 (count data)))]
    (.setString retval 0 data "ASCII")
    retval))


(defn string->ptr-untracked
  ^Pointer [^String data]
  (let [^Pointer retval (malloc-untracked (+ 1 (count data)))]
    (.setString retval 0 data "ASCII")
    retval))


(defn string->wide-ptr
  ^Pointer [^String data]
  (let [^Pointer retval (malloc (-> (+ 1 (count data))
                                    (* Native/WCHAR_SIZE)))]
    (.setWideString retval 0 data)
    retval))


(defn wide-ptr->string
  ^String [^Pointer wide-ptr]
  (.getWideString wide-ptr 0))


(defn create-ptr-ptr
  "Create a pointer to a pointer."
  [^Pointer ptr]
  (let [ptr-map {:ptr ptr}
        retval (PointerByReference. ptr)]
    ;;Ensure the original ptr is referenced else you could get hurt.
    (resource/track retval #(get ptr-map :ptr) [:gc])))


(defn checknil
  ^Pointer [value]
  (let [value (->ptr-backing-store value)]
    (if (= 0 (long (Pointer/nativeValue value)))
      (throw (ex-info "Pointer value is nil"
                      {}))
      value)))


(defn ensure-type
  [item-cls item]
  (base/ensure-type item-cls item))


(defn ensure-ptr-ptr
  ^PointerByReference [item]
  (base/ensure-ptr-ptr item))


(defn ensure-ptr
  ^Pointer [item]
  (base/ensure-ptr (->ptr-backing-store
                    item)))


(defn size-t
  [& [item]]
  (case Native/SIZE_T_SIZE
    4 (int (or item 0))
    8 (long (or item 0))))


(def size-t-type (type (size-t)))


(def size-t-ref-type (if (= Long size-t-type)
                       LongByReference
                       IntByReference))


(defn size-t-ref
  [& [init-value]]
  (if (= LongByReference size-t-ref-type)
    (LongByReference. (long (or init-value 0)))
    (IntByReference. (int (or init-value 0)))))


(defn size-t-ref-value
  [ref-obj]
  (if (instance? LongByReference ref-obj)
    (.getValue ^LongByReference ref-obj)
    (.getValue ^IntByReference ref-obj)))


(defmacro def-jna-fn
  "Define a dynamically bound fn.  Upon first call, it will attempt to find
  the function pointer from libname.
  Argpair is of type [symbol type-coersion], symbol cannot match type-coersion."
  [libname fn-name docstring rettype & argpairs]
  `(base/def-jna-fn ~libname ~fn-name ~docstring ~rettype ~@argpairs))


(defn c-library-name
  ^String []
  (base/c-library-name))


(defn math-library-name
  ^String []
  (base/math-library-name))
