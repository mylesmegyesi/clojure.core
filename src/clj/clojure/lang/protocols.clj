(ns clojure.lang.protocols
  (:refer-clojure :only [defprotocol]))

(defprotocol IAgent
  (-action-queue [this])
  (-dispatch [this f args executor])
  (-error-handler [this])
  (-error-mode [this])
  (-enqueue [this action])
  (-set-state [this new-state]))

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

(defprotocol IDecimal)

(defprotocol IDeref
  (-deref [this]))

(defprotocol IEditableCollection
  (-as-transient [this]))

(defprotocol IEquivalence
  "This protocol should not be directly used in a deftype. It should only
  be used to override an existing platform equality method."
  (-equal?      [this other])
  (-equivalent? [this other]))

(defprotocol IFloat)

(defprotocol IHash
  "This protocol should not be directly used in a deftype. It should only
  be used to override an existing platform hash method."
  (-hash [this]))

(defprotocol IIndexed
  (-nth [this n] [this n not-found]))

(defprotocol IInteger)

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

(defprotocol IPersistentCollection
  (-cons [this x])
  (-empty [this]))

(defprotocol IPersistentStack
  (-peek [this])
  (-pop [this]))

(defprotocol IPersistentList)

(defprotocol IPersistentMap
  (-dissoc [this k]))

(defprotocol IPersistentSet
  (-conj         [this xs])
  (-difference   [this sets])
  (-disj         [this xs])
  (-intersection [this sets])
  (-union        [this sets]))

(defprotocol IPersistentVector
  (-assoc-n [this n x]))

(defprotocol IRatio
  (-numerator [this])
  (-denominator [this]))

(defprotocol ISeq
  (-first [this])
  (-next  [this])
  (-more [this]))

(defprotocol ISeqable
  (-seq [this]))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")

(defprotocol IShow
  (-show [this]))

(defprotocol ITransientCollection
  (-conj! [this value])
  (-persistent [this]))

(defprotocol IValidatable
  (-get-validator  [this])
  (-set-validator! [this validator-fn]))

(defprotocol IWatchable
  (-get-watches    [this])
  (-add-watch      [this watch-key callback-fn])
  (-remove-watch   [this watch-key])
  (-notify-watches [this old-val new-val]))

