(ns clojure.lang.protocols
  (:refer-clojure :only [defprotocol]))

(defprotocol IAssociative
  (-contains-key? [this k])
  (-entry-at      [this k])
  (-assoc         [this k v]))

(defprotocol IAtom
  (-compare-and-set! [this old-state new-state])
  (-reset! [this new-state])
  (-swap! [this f args]))

(defprotocol IComparable
  "This protocol should not be directly used in a deftype. It should only
  be used to override an existing platform compare-to method."
  (-compare-to [this other]))

(defprotocol ICounted
  (-count [this]))

(defprotocol IDeref
  (-deref [this]))

(defprotocol IEquivalence
  "This protocol should not be directly used in a deftype. It should only
  be used to override an existing platform equality method."
  (-equal?      [this other])
  (-equivalent? [this other]))

(defprotocol IHash
  "This protocol should not be directly used in a deftype. It should only
  be used to override an existing platform hash method."
  (-hash [this]))

(defprotocol ILookup
  (-includes? [this k])
  (-lookup [this k not-found]))

(defprotocol IMapEntry
  (-key [this])
  (-val [this]))

(defprotocol IMeta
  (-meta [this])
  (-with-meta [this new-meta])
  (-reset-meta! [this new-meta])
  (-alter-meta! [this f args]))

(defprotocol INamed
  (-name [this])
  (-namespace [this]))

(defprotocol IPersistentMap
  (-dissoc [this k]))

(defprotocol IPersistentSet
  (-conj         [this xs])
  (-difference   [this sets])
  (-disj         [this xs])
  (-intersection [this sets])
  (-union        [this sets]))

(defprotocol IRatio
  (-numerator [this])
  (-denominator [this]))

(defprotocol ISeq
  (-first [this])
  (-next  [this]))

(defprotocol ISeqable
  (-seq [this]))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")

(defprotocol IShow
  (-show [this]))

(defprotocol IValidatable
  (-get-validator  [this])
  (-set-validator! [this validator-fn]))

(defprotocol IWatchable
  (-add-watch    [this watch-key callback-fn])
  (-remove-watch [this watch-key]))
