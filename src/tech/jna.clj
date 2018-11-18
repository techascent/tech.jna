(ns tech.jna
  (:require [tech.jna.base :as base]
            [tech.gc-resource :as gc-resource])
  (:import [com.sun.jna Native NativeLibrary Pointer Function Platform]
           [com.sun.jna.ptr PointerByReference]))



(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


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
  (let [retval (malloc-untracked num-bytes)]
    (gc-resource/track retval #(Native/free (Pointer/nativeValue retval)))
    retval))


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
