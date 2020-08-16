(ns lcc.board
  (:require [clojure.spec.alpha :as s]
            [ghostwheel.core :refer [>defn >defn-]]))

(def ^:private empty-board
  {:player-1 {:yellow [] :white [] :green [] :blue [] :red [] :hand []}
   :player-2 {:yellow [] :white [] :green [] :blue [] :red [] :hand []}
   :board {:yellow [] :white [] :green [] :blue [] :red [] :draw-pile []}})

(def ^:private full-deck
  [{:suit :yellow :value :wager} {:suit :yellow :value :wager} {:suit :yellow :value :wager}
   {:suit :yellow :value 2} {:suit :yellow :value 3} {:suit :yellow :value 4}
   {:suit :yellow :value 5} {:suit :yellow :value 6} {:suit :yellow :value 7}
   {:suit :yellow :value 8} {:suit :yellow :value 9} {:suit :yellow :value 10}
   {:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value :wager}
   {:suit :white :value 2} {:suit :white :value 3} {:suit :white :value 4}
   {:suit :white :value 5} {:suit :white :value 6} {:suit :white :value 7}
   {:suit :white :value 8} {:suit :white :value 9} {:suit :white :value 10}
   {:suit :green :value :wager} {:suit :green :value :wager} {:suit :green :value :wager}
   {:suit :green :value 2} {:suit :green :value 3} {:suit :green :value 4}
   {:suit :green :value 5} {:suit :green :value 6} {:suit :green :value 7}
   {:suit :green :value 8} {:suit :green :value 9} {:suit :green :value 10}
   {:suit :blue :value :wager} {:suit :blue :value :wager} {:suit :blue :value :wager}
   {:suit :blue :value 2} {:suit :blue :value 3} {:suit :blue :value 4}
   {:suit :blue :value 5} {:suit :blue :value 6} {:suit :blue :value 7}
   {:suit :blue :value 8} {:suit :blue :value 9} {:suit :blue :value 10}
   {:suit :red :value :wager} {:suit :red :value :wager} {:suit :red :value :wager}
   {:suit :red :value 2} {:suit :red :value 3} {:suit :red :value 4}
   {:suit :red :value 5} {:suit :red :value 6} {:suit :red :value 7}
   {:suit :red :value 8} {:suit :red :value 9} {:suit :red :value 10}])

(>defn- draw-n
  "Draw x cards. Return a tuple of the drawn cards and the rest of the deck."
  [deck n]
  [vector? int? :ret (s/tuple vector? vector?)]
  [(take n deck) (drop n deck)])

(>defn set-up-game
  "Sets up the game ready to go."
  []
  [:ret map?]
  (let [starting-deck (shuffle full-deck)
        [hand-1 starting-deck] (draw-n starting-deck 8)
        [hand-2 starting-deck] (draw-n starting-deck 8)]
    (-> empty-board
        (assoc-in [:player-1 :hand] hand-1)
        (assoc-in [:player-2 :hand] hand-2)
        (assoc-in [:board :draw-pile] starting-deck))))

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