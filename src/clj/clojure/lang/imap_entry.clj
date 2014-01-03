(ns clojure.lang.imap-entry
  (:refer-clojure :only [defprotocol]))

(defprotocol IMapEntry
  (-key [this])
  (-val [this]))
