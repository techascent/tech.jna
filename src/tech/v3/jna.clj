(ns tech.v3.jna
  "Simple bindings to the JNA system.  Fastest pathway to success is `def-jna-fn`.
  Note that the default behavior for malloc has changed; the default resource type
  is :gc now as opposed to [:stack :gc].

  Also, for ease of use when creating derived objects from gc-managed native
  pointer-based objects see
  [`tech.v3.resource/chain-resources`](https://techascent.github.io/tech.resource/tech.v3.resource.html#var-chain-resources).
  "
  (:require [tech.v3.jna.base :as base]
            [tech.v3.resource :as resource])
  (:import [com.sun.jna Native NativeLibrary Pointer Function Platform Structure]
           [com.sun.jna.ptr PointerByReference LongByReference IntByReference]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defprotocol PToPtr
  (is-jna-ptr-convertible? [item]
    "Is this object convertible to a JNA pointer?")
  (->ptr-backing-store [item]
    "Conversion to a jna pointer type that points to the data of the object."))


(extend-type Object
  PToPtr
  (is-jna-ptr-convertible? [item] false))


(defn ptr-convertible?
  "Is this object pointer convertible via the PToPtr protocol."
  [item]
  (is-jna-ptr-convertible? item))


(defn as-ptr
  "Convert this object to a jna Pointer returning nil if not possible.  For a checked
  conversion see `ensure-ptr`."
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
  "Load a library.  Returns "
  ^NativeLibrary [libname]
  (base/load-library libname))


(defn find-function
  "Given a function name and a library name, find the function in the library.
  Returns a com.sun.jna Function object.  For a much more user-friendly and higher
  level pathway see `def-jna-fn`"
  ^Function [fn-name libname]
  (base/find-function fn-name libname))


(defn malloc
  "Malloc a pointer of Y bytes.  Track using both resource context
  and gc system.

  Options:

   * `:resource-type` - Defaults to `:gc`.  May be one or both of
      * `:gc` - rely on the garbage collector to let us know when the ptr is no
         longer reachable by our program.
      * `:stack` - Must be used within a tech.v3.resource/stack-resource-context and
         ensures the memory will be freed when the nearest scope has exited.

  For a much more thorough treatment of native heap data, please see the documentation
  for [dtype-next](https://cnuernber.github.io/dtype-next/)."
  (^Pointer [^long num-bytes {:keys [resource-type]
                              :or {resource-type #{:gc}}}]
   (let [retval (Pointer. (Native/malloc num-bytes))
         native-value (Pointer/nativeValue retval)]
     (if resource-type
       (resource/track retval
                       {:dispose-fn #(Native/free native-value)
                        :track-type resource-type})
       retval)))
  (^Pointer [^long num-bytes]
   (malloc num-bytes nil)))


(defn malloc-untracked
  "Malloc pointer of Y bytes.  Up to caller to call Native/free on result at some
  point"
  ^Pointer [^long num-bytes]
  (malloc num-bytes {:resource-type nil}))


(defn unsafe-read-byte
  "Read a byte from pointer byte-ary at address idx.  For bulk access convert the
  pointer to a `tech.v3.datatype.native-buffer/NativeBuffer` via:

```clojure
  (tech.v3.datatype.native-buffer/wrap-address
     (com.sun.jna.Pointer/nativeValue ptr)
     ...)
```"
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
  "Convert a string to a pointer.  Track the data via the gc; when the Pointer
  goes out of scope the memory will be freed."
  (^Pointer [^String data options]
   (let [^Pointer retval (malloc (+ 1 (count data)) options)]
     (.setString retval 0 data "ASCII")
     retval))
  (^Pointer [^String data]
   (string->ptr data nil)))


(defn string->ptr-untracked
  "Convert a string to a pointer.  Memory will not be automatically freed."
  ^Pointer [^String data]
  (string->ptr data {:resource-type nil}))


(defn string->wide-ptr
  "Convert a string into a wchar-t using utf-16."
  (^Pointer [^String data options]
   (let [^Pointer retval (malloc (-> (+ 1 (count data))
                                     (* Native/WCHAR_SIZE))
                                 options)]
     (.setWideString retval 0 data)
     retval))
  (^Pointer [data]
   (string->wide-ptr data)))


(defn wide-ptr->string
  "Convert a wchar-t ptr to a java string"
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
  "Check that a thing is a pointer and the integer value is not zero."
  ^Pointer [value]
  (let [value (->ptr-backing-store value)]
    (if (= 0 (long (Pointer/nativeValue value)))
      (throw (ex-info "Pointer value is nil"
                      {}))
      value)))


(defn ensure-type
  "Ensure a thing is derived from item-cls"
  [item-cls item]
  (base/ensure-type item-cls item))


(defn ensure-ptr-ptr
  "Ensure a thing is a ptr-to-a-ptr."
  ^PointerByReference [item]
  (base/ensure-ptr-ptr item))


(defn ensure-ptr
  "Convert thing to pointer or throw exception."
  ^Pointer [item]
  (base/ensure-ptr (->ptr-backing-store
                    item)))

(defmacro size-t-compile-time-switch
  "Run either int32 based code or int64 based code depending
   on the runtime size of size-t"
  [int-body long-body]
  (case Native/SIZE_T_SIZE
    4 `~int-body
    8 `~long-body))


(defn size-t
  "Convert item to either an integer or a long depending on the size of
  size-t."
  ([item]
   (size-t-compile-time-switch (int item) (long item)))
  ([]
   (size-t-compile-time-switch (int 0) (long 0))))


(def ^{:doc "The runtime class type of a c size-t"} size-t-type (type (size-t)))


(def ^{:doc "The runtime reference-by-ptr type of a c size-t"}
  size-t-ref-type (if (= Long size-t-type)
                    LongByReference
                    IntByReference))


(defn size-t-ref
  "Create a reference to a size-t."
  ([]
   (size-t-compile-time-switch
    (IntByReference. (int 0))
    (LongByReference. (long 0))))
  ([item]
   (size-t-compile-time-switch
    (IntByReference. (int item))
    (LongByReference. (long item)))))


(defn size-t-ref-value
  "Get the value from a size-t reference."
  [ref-obj]
  (if (instance? LongByReference ref-obj)
    (.getValue ^LongByReference ref-obj)
    (.getValue ^IntByReference ref-obj)))


(defmacro def-jna-fn
  "Define a dynamically bound fn.  Upon first call, it will attempt to find
  the function pointer from libname.  Redefinition resets the functions so it
  will rebind to a location.


  * `rettype` - Class return type or nil for void.
  * `argpairs` - one or more pairs of type [symbol type-coersion] where the symbol
     is what will be displayed in the function's docstring and type-coersion is a
     function that is run at function call time to ensure the type is the exact
     correct type.  If coersion function is wrong and creates the wrong type for
     the function signature your program will probably crash."
  [libname fn-name docstring rettype & argpairs]
  `(base/def-jna-fn ~libname ~fn-name ~docstring ~rettype ~@argpairs))


(defn c-library-name
  "Get the c library name for your system.  This can be used with def-jna-fn
  to bind to various c library function calls."
  ^String []
  (base/c-library-name))


(defn math-library-name
  "Get the c math library name for your system.  This can be used with def-jna-fn
  to bind to various c library function calls."
  ^String []
  (base/math-library-name))


(defn register-direct-mapped-class!
  "Register a direct-mapped class with a library.  Calling direct-mapped functions
  *dramatically* decreases function call overhead and brings it inline with hand-built
  JNI bindings.

  Direct-mapped classes look like normal classes with functions defined with
  [`static native` attributes](https://github.com/clj-python/libpython-clj/blob/c4d0c2cb6476d053013224cf8b441f1f55241eee/java/libpython_clj/jna/DirectMapped.java).

  From [libpython-clj](https://github.com/clj-python/libpython-clj/blob/c4d0c2cb6476d053013224cf8b441f1f55241eee/src/libpython_clj/python/interpreter.clj#L431):

```clojure
(com.sun.jna.Native/register DirectMapped library)
```"
  [libname cls-obj]
  (->> (load-library libname)
       (Native/register ^Class cls-obj)))


(defn reload!
  "Reload a shared library.  This means that any dynamically bound functions
  defined via `def-jna-fn` will load the new library's functions as they
  are dynamically found at call time every time.  Any direct-mapped classes
  will need to be rebound via register-direct-mapped-class."
  [libname]
  (when-let [^NativeLibrary native-lib (get @base/*loaded-libraries* libname)]
    (.dispose native-lib)
    (swap! base/*loaded-libraries* dissoc libname)
    (base/do-load-library libname)))
