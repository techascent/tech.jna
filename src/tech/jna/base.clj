(ns tech.jna.base
  (:require [clojure.java.io]
            [clojure.string :as s])
  (:import [com.sun.jna Native NativeLibrary Pointer Function Platform]
           [com.sun.jna.ptr PointerByReference]
           [java.lang.reflect Method]
           [java.io File]))


;;Moved into future due to loading time issues.
;;This takes at least 1 second alone if not longer.
(def taoensso-logger (future
                       (try (require '[tech.jna.timbre-log])
                            (resolve 'tech.jna.timbre-log/log-info)
                            (catch Throwable e
                              nil))))


(defn log-info
  [log-str]
  (if-let [logger @taoensso-logger]
    (logger log-str)
    (println log-str)))


(defn- log-load-attempt
  [libname pathtype path success?]
  (let [log-str (format "Attempting to load %s: %s %s - %s"
                        libname pathtype path (if success?
                                                "succeeded!"
                                                "failed"))]
    (log-info log-str)))


(def native-lib-methods
  (->> (.getDeclaredMethods NativeLibrary)
       (map (fn [^Method method]
              {:name (.getName method)
               :method method}))
       (group-by :name)))


(def map-shared-libname-native
  (-> (get native-lib-methods "mapSharedLibraryName")
      first
      :method
      ((fn [^Method method]
         (.setAccessible method true)
         method))))


(defn map-shared-library-name
  [libname]
  (.invoke ^Method map-shared-libname-native
           NativeLibrary (object-array [libname])))


(def ^:dynamic *library-name->paths* (atom {}))


(defn add-library-path
  "Path order has to be explicitly controlled by the user."
  [libname path-type path]
  (swap! *library-name->paths*
         update libname
         (fn [existing-paths]
           (if-not existing-paths
             [[path-type path]]
             (->> (conj existing-paths [path-type path])
                  distinct
                  vec)))))


(defn clear-library-paths
  [libname]
  (swap! *library-name->paths* dissoc libname))


(defn library-paths
  [libname]
  (get @*library-name->paths* libname))


(defn add-resource-path
  "Look in resources for library"
  [libname & [libpath class-loader]]
  (add-library-path libname :resource
                    {:path (or libpath libname)
                     ;;Uses current thread classloader if non provided.
                     :class-loader class-loader}))


(defn add-java-library-path-path
  [libname & [libpath]]
  (add-library-path libname :java-library-path (or libpath libname)))


(defmulti expand-pathtype
  (fn [pathtype & args]
    pathtype))



(defmethod expand-pathtype :default
  [pathtype path]
  path)


;;No mangling, jna takes care of this.
(defmethod expand-pathtype :system
  [pathtype path]
  path)


(defmethod expand-pathtype :resource
  [pathtype {:keys [path class-loader]}]
  (let [^File file
        (try
          (if class-loader
            (Native/extractFromResourcePath ^String path class-loader)
            (Native/extractFromResourcePath ^String path))
          (catch Throwable e
            (log-info (format "Failed to find library %s as a resource" path))))]
    (if file
      (.getCanonicalPath file)
      path)))


(defmethod expand-pathtype :java-library-path
  [pathtype path]
  (let [path (map-shared-library-name path)]
    (->> (s/split (System/getProperty "java.library.path") #":")
         (map #(str % File/separator path)))))


(defn do-load-library
  [libname]
  (let [pathtypes (get @*library-name->paths* libname)
        pathtypes (if-not (seq pathtypes)
                    [[:system libname]
                     [:java-library-path libname]]
                    pathtypes)
        ;;Dedup but preserve order
        path-order (->> pathtypes
                        (map first)
                        distinct)
        deduped (->> pathtypes
                     (group-by first)
                     (map (fn [[pathtype pathtype-pairs]]
                            [pathtype (->> pathtype-pairs
                                           (map second)
                                           distinct)]))
                     (into {}))
        paths (->> path-order
                   (mapcat (fn [pathtype]
                             (->> (get deduped pathtype)
                                  (mapcat (fn [pathtype-path]
                                            (let [expanded (expand-pathtype pathtype pathtype-path)]
                                              (if (string? expanded)
                                                [[pathtype expanded]]
                                                (map #(vector pathtype %) expanded)))))))))
        retval (->> paths
                    (map (fn [[pathname load-path]]
                           (try
                             [pathname load-path
                              (NativeLibrary/getInstance load-path)]
                             (catch Throwable e))))
                    (remove nil?)
                    first)]
    (when-not retval
      (throw (ex-info "Failed to load library"
                      {:libname libname
                       :paths (let [pathmap (->> (group-by first paths)
                                                 (map #(vector (first %)
                                                               (mapv second (second %))))
                                                 (into {}))]
                                (->> path-order
                                     (mapv #(vector % (get pathmap %)))))})))

    (log-info (format "Library %s found at %s" libname [(first retval) (second retval)]))
    (last retval)))



(def ^:dynamic *loaded-libraries* (atom {}))


(defn load-library
  ^NativeLibrary [libname]
  (if-let [retval (get @*loaded-libraries* libname)]
    retval
    (do
      (let [retval (do-load-library libname)]
        (swap! *loaded-libraries* assoc libname retval)
        retval))))


(defn jar-native-library-path
  "If you want java-library-path setup for you you need to put
native libraries under this location:"
  []
  (let [resource-path Platform/RESOURCE_PREFIX
        first-dash-idx (.indexOf resource-path "-")]
    (str "native"
         File/separator
         (.substring resource-path 0 first-dash-idx)
         File/separator
         (.substring resource-path (+ first-dash-idx 1)))))


(defn ensure-type
  [item-cls item]
  (when-not (instance? item-cls item)
    (throw (ex-info "Item is not desired type"
                    {:item-cls item-cls
                     :item item})))
  item)


(defn set-loaded-library!
  [libname native-library]
  (ensure-type NativeLibrary native-library)
  (swap! *loaded-libraries* assoc libname native-library))


(def do-find-function
  (fn [fn-name libname]
    (.getFunction ^NativeLibrary (load-library libname) fn-name)))


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


(defn char-ptr-ptr->string-vec
  "Decode a char** ptr."
  [^long num-strings ^Pointer char-ptr-ptr]
  (let [base-address (Pointer/nativeValue char-ptr-ptr)]
    (->> (range num-strings)
         (map (fn [name-idx]
                (let [new-ptr (-> (+ (* (long name-idx) Native/POINTER_SIZE)
                                     base-address)
                                  (Pointer.))
                      char-ptr (case Native/POINTER_SIZE
                                 8 (.getLong new-ptr 0)
                                 4 (.getInt new-ptr 0))]
                  (variable-byte-ptr->string (Pointer. char-ptr)))))
         ;;Don't be lazy.  the ptr may be released.
         vec)))


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


(defn c-library-name
  ^String []
  (Platform/C_LIBRARY_NAME))


(defn math-library-name
  ^String []
  (Platform/MATH_LIBRARY_NAME))
