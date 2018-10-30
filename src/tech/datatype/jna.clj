(ns tech.datatype.jna
  (:require [tech.datatype.base :as dtype-base]
            [tech.datatype.java-primitive :as primitive]
            [tech.datatype.java-unsigned :as unsigned]
            [tech.datatype :as dtype]
            [tech.jna :as jna]
            [tech.resource :as resource]
            [clojure.core.matrix.protocols :as mp])
  (:import [com.sun.jna Pointer Native Function NativeLibrary]
           [java.nio ByteBuffer]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)



(jna/def-jna-fn "c" memset
  "Set a block of memory to a value"
  Pointer
  [data jna/ensure-ptr]
  [val int]
  [num-bytes int])


(defprotocol PToPtr
  (->ptr-backing-store [item]))


(defn- integer-datatype?
  [datatype]
  (boolean (#{:int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64} datatype)))


(defn offset-pointer
  ^Pointer [^Pointer ptr, ^long offset]
  (Pointer. (+ offset (Pointer/nativeValue ptr))))


(defn make-jna-pointer
  "Use with care..."
  ^Pointer [^long address]
  (Pointer. address))


(defn pointer->address
  ^long [^Pointer ptr]
  (Pointer/nativeValue ptr))


(defrecord TypedPointer [^Pointer ptr ^long byte-len datatype]
  PToPtr
  (->ptr-backing-store [item] ptr)

  dtype-base/PDatatype
  (get-datatype [item] datatype)

  dtype-base/PAccess
  (set-value! [item offset value]
    (dtype-base/set-value! (unsigned/->typed-buffer item)
                           offset value))
  (set-constant! [item offset value elem-count]
    (when (< (long offset) 0 )
      (throw (ex-info "Offset out of range!"
                      {:offset offset})))
    (let [lval (long value)]
      (when-not (<= (long elem-count)
                    (- (dtype-base/ecount item)
                       (long offset)))
        (throw (ex-info "Element count out of range"
                        {:offset offset
                         :item-ecount (dtype-base/ecount item)
                         :elem-count elem-count})))
      (if (or (= value 0)
              (or (and (= :uint8 datatype)
                       (>= lval 0)
                       (<= lval 255))
                  (and (= :int8 datatype)
                       (>= lval Byte/MIN_VALUE)
                       (<= lval Byte/MAX_VALUE))))
        (let [byte-size (dtype-base/datatype->byte-size datatype)
              offset-bytes (* byte-size (long offset))
              n-elems-bytes (* byte-size (long elem-count))]
          (memset (offset-pointer ptr offset-bytes) (dtype-base/cast value datatype) n-elems-bytes))
        (dtype-base/set-constant! (unsigned/->typed-buffer item)
                                  offset value elem-count))))
  (get-value [item offset]
    (dtype-base/get-value (unsigned/->typed-buffer item) offset))

  mp/PElementCount
  (element-count [_] (quot byte-len
                           (dtype-base/datatype->byte-size datatype)))
  dtype-base/PContainerType
  (container-type [item] :jna-buffer)

  dtype-base/PCopyRawData
  (copy-raw->item! [raw-data ary-target target-offset options]
    (dtype-base/copy-raw->item! (unsigned/->typed-buffer raw-data) ary-target
                                target-offset options))
  primitive/PToBuffer
  (->buffer-backing-store [item]
    (let [jvm-type (unsigned/datatype->jvm-datatype datatype)
          buffer (.getByteBuffer ptr 0 byte-len)]
      (case jvm-type
        :int8 buffer
        :int16 (.asShortBuffer buffer)
        :int32 (.asIntBuffer buffer)
        :int64 (.asLongBuffer buffer)
        :float32 (.asFloatBuffer buffer)
        :float64 (.asDoubleBuffer buffer))))

  primitive/PToArray
  (->array [item] nil)
  (->array-copy [item] (primitive/->array-copy
                        (unsigned/->typed-buffer item))))


;;If you are a typed-pointer then you can advertise that you support the jna-buffer container type.
;;If you are not, then do not.
(defn typed-pointer?
  [item]
  (and (unsigned/typed-buffer? item)
       (satisfies? PToPtr item)))


(defn as-typed-pointer
  "Get something substitutable as a typed-pointer.
Implement all the protocols necessary to be tech.datatype.java-unsigned/typed-buffer
*and* PToPtr and you can be considered a typed-pointer."
  [item]
  (when (typed-pointer? item)
    item))


(defn ->typed-pointer
  "Creates a typed-pointer object.
  Implement PToPtr, mp/PElementCount, and dtype-base/get-datatype
and we convert your thing to a typed pointer."
  [item]
  ;;Implement 3 protocols and we provide the conversion.
  (let [ptr-data (->ptr-backing-store item)
        ptr-dtype (dtype-base/get-datatype item)
        num-bytes (* (dtype-base/ecount item)
                     (dtype-base/datatype->byte-size ptr-dtype))]
    (->TypedPointer ptr-data num-bytes ptr-dtype)))


(defn typed-pointer->ptr
  ^Pointer [typed-pointer]
  (->ptr-backing-store typed-pointer))


(dtype-base/add-container-conversion-fn
 :jna-buffer :typed-buffer
 (fn [dest-type jna-buf]
   [(unsigned/->typed-buffer jna-buf)
    0]))


(defn unsafe-address->typed-pointer
  [^long address ^long byte-len datatype]
  (->TypedPointer (Pointer. address) byte-len datatype))


(defmacro typed-data-setter
  [datatype set-fn ptr item-seq]
  `(let [byte-size# (dtype-base/datatype->byte-size ~datatype)]
     (->> ~item-seq
          (map-indexed (fn [idx# val#]
                         (. ~ptr ~set-fn (* (long idx#) byte-size#)
                            (primitive/datatype->unchecked-cast-fn :ignored ~datatype val#))))
          dorun)))


(defn make-typed-pointer
  [datatype elem-count-or-seq & [options]]
  (let [n-elems (long (if (number? elem-count-or-seq)
                        elem-count-or-seq
                        (count elem-count-or-seq)))
        elem-count-or-seq (unsigned/unsigned-safe-elem-count-or-seq
                           datatype elem-count-or-seq options)
        byte-len (* n-elems (dtype-base/datatype->byte-size datatype))
        data (Native/malloc byte-len)
        _ (resource/make-resource #(Native/free data))
        retval (unsafe-address->typed-pointer data byte-len datatype)
        jvm-datatype (unsigned/datatype->jvm-datatype datatype)
        ptr-data (typed-pointer->ptr retval)
        data-ary (dtype/make-array-of-type jvm-datatype elem-count-or-seq {:unchecked? true})]
    (dtype/copy! data-ary 0 retval 0 n-elems {:unchecked? true})
    retval))


(jna/def-jna-fn "c" memcpy
  "Copy bytes from one object to another"
  Pointer
  [dst jna/ensure-ptr]
  [src jna/ensure-ptr]
  [n-bytes int])


(defmacro ^:private array->buffer-copy
  [datatype]
  (let [jvm-dtype (unsigned/datatype->jvm-datatype datatype)]
    `(fn [src# src-offset# dst# dst-offset# n-elems# options#]
       (let [src# (primitive/datatype->array-cast-fn ~jvm-dtype src#)
             dst-ptr# (typed-pointer->ptr dst#)]
         (.write dst-ptr# (int dst-offset#) src# (int src-offset#) (int n-elems#))))))


(defmacro ^:private buffer->array-copy
  [datatype]
  (let [jvm-dtype (unsigned/datatype->jvm-datatype datatype)]
    `(fn [src# src-offset# dst# dst-offset# n-elems# options#]
       (let [src# (typed-pointer->ptr src#)
             dst# (primitive/datatype->array-cast-fn ~jvm-dtype dst#)]
         (.read src# (int src-offset#) dst# (int dst-offset#) (int n-elems#))))))


(defn- buffer->buffer-copy
  [src src-offset dst dst-offset n-elems options]
  (let [src-dtype (dtype-base/get-datatype src)
        dst-dtype (dtype-base/get-datatype dst)]
    (if (or (= src-dtype dst-dtype)
            (and (:unchecked? options)
                 (unsigned/direct-conversion? src-dtype dst-dtype)))
      (let [src (typed-pointer->ptr src)
            dst (typed-pointer->ptr dst)
            byte-size (dtype-base/datatype->byte-size src-dtype)
            _ (when-not (= byte-size (dtype-base/datatype->byte-size dst-dtype))
                (throw (ex-info "src/dst datatype size mismatch"
                                {:src-datatype src-dtype
                                 :dst-datatype dst-dtype})))
            src-byte-offset (* (long src-offset) byte-size)
            dst-byte-offset (* (long dst-offset) byte-size)
            copy-byte-len (* (long n-elems) byte-size)]
        (memcpy (offset-pointer dst dst-byte-offset)
                (offset-pointer src src-byte-offset)
                copy-byte-len))
      (dtype-base/copy! (unsigned/->typed-buffer src) src-offset
                        (unsigned/->typed-buffer dst) dst-offset
                        n-elems options))))

;;Add array to buffer and back fast paths

(defmacro array-copy-fns
  []
  (->> (for [dtype unsigned/datatypes]
         (let [jvm-dtype (unsigned/datatype->jvm-datatype dtype)]
           (if (= dtype jvm-dtype)
             `(let [to-copy-fn# (array->buffer-copy ~dtype)
                    from-copy-fn# (buffer->array-copy ~dtype)]
                (dtype-base/add-copy-operation :java-array :jna-buffer ~dtype ~dtype true to-copy-fn#)
                (dtype-base/add-copy-operation :java-array :jna-buffer ~dtype ~dtype false to-copy-fn#)
                (dtype-base/add-copy-operation :jna-buffer :java-array ~dtype ~dtype true from-copy-fn#)
                (dtype-base/add-copy-operation :jna-buffer :java-array ~dtype ~dtype false from-copy-fn#)
                ~dtype)
             `(let [to-copy-fn# (array->buffer-copy ~dtype)
                    from-copy-fn# (buffer->array-copy ~dtype)]
                (dtype-base/add-copy-operation :java-array :jna-buffer ~jvm-dtype ~dtype true to-copy-fn#)
                (dtype-base/add-copy-operation :jna-buffer :java-array ~dtype ~jvm-dtype true from-copy-fn#)
                ~dtype))))
       vec))

(def ^:private primitive-array-copy (array-copy-fns))


(def ^:private buffer-copy
  (->> (for [[src-dtype dst-dtype] unsigned/all-possible-datatype-pairs
             unchecked? [true false]]
         (do
           (dtype-base/add-copy-operation :jna-buffer :jna-buffer src-dtype dst-dtype unchecked?
                                          buffer->buffer-copy)
           [src-dtype dst-dtype unchecked?]))
       vec))
