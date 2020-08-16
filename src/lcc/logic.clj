(ns lcc.logic
  (:require [ghostwheel.core :refer [>defn]]))

(>defn allowed-play?
  "Test whether a play is allowed."
  [path new-card]
  [vector? map? :ret boolean?]
  (if (seq path)
    (let [last-card (peek path)]
      (and (= (:suit last-card) (:suit new-card))
           (or (keyword? (:value last-card))
               (< (:value last-card) (:value new-card)))))
    true))

(comment
  (allowed-play? [] {:suit :red :value 2})
  (allowed-play? [{:suit :red :value :wager}] {:suit :red :value 2})
  (allowed-play? [{:suit :red :value :wager} {:suit :red :value 4}] {:suit :red :value 2}))