(ns lcc.logic
  (:require [clojure.spec.alpha :as s]
            [ghostwheel.core :refer [>defn >defn-]]))

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

(>defn- score-path
  "Scores a single path in the game. Assumes a valid path."
  [path]
  [sequential? :ret int?]
  (if (seq path)
    (let [wager-multiplier (inc (count (filter #(= :wager (:value %)) path)))
          point-cards (remove #(= :wager (:value %)) path)
          bonus-points (if (<= 8 (count path)) 20 0)]
      (->> point-cards
           (map :value)
           (apply + -20)
           (* wager-multiplier)
           (+ bonus-points)))
    0))

(comment
  (score-path [{:suit :red :value :wager} {:suit :red :value :wager}])
  ;; Expected: -60
  (score-path [{:suit :yellow :value 5} {:suit :yellow :value 8} {:suit :yellow :value 10}])
  ;; Expected: 3
  (score-path [])
  ;; Expected: 0
  (score-path [{:suit :green :value :wager} {:suit :green :value 3} {:suit :green :value 5} {:suit :green :value 7}])
  ;; Expected: -10
  (score-path [{:suit :red :value :wager} {:suit :red :value :wager} {:suit :red :value 2} {:suit :red :value 3}
               {:suit :red :value 5} {:suit :red :value 7} {:suit :red :value 8} {:suit :red :value 10}])
  ;; Expected: 65
  )

(>defn score-game
  "Calculate scores for both players."
  [board]
  [map? :ret (s/coll-of int?)]
  ())

