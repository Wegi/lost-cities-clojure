(ns lcc.board
  (:require [clojure.spec.alpha :as s]
            [ghostwheel.core :refer [>defn >defn- ?]]
            [lcc.logic :as logic]))

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
  [(take n deck) (vec (drop n deck))])

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

(>defn add-to-discard
  "Discard a card to the board. Returns the new board."
  [state card]
  [map? map? :ret map?]
  (update-in state [:board (:suit card)] conj card))

(>defn add-to-path
  "Plays a card onto the players side if the path allows it."
  [state card player]
  [map? map? keyword? :ret (? map?)]
  (let [suit (:suit card)
        path (get-in state [player suit])]
    (when (logic/allowed-play? path card)
      (update-in state [player suit] conj card))))

(>defn draw
  "A player draws a card into their hand."
  [state player]
  [map? keyword? :ret map?]
  (let [deck-before-draw (get-in state [:board :draw-pile])
        [[new-card] rest-deck] (draw-n deck-before-draw 1)]
    (-> state
        (update-in [player :hand] conj new-card)
        (assoc-in [:board :draw-pile] rest-deck))))
