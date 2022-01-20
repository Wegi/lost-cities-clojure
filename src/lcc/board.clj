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

(def ^:private card-value-padding
  ["  " "  " "  " "  " "  " "  " "  "])

(def ^:private colors
  [:yellow :blue :white :green :red])

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

(>defn- value-for-print
  "Convert the values for print. (Padding and abbreviations)"
  [value]
  [(? (s/or :n int? :k keyword?)) :ret string?]
  (case value
    :wager ":W"
    10 "10"
    nil "  "
    (str " " value)))

(>defn- print-player!
  "Prints the side of `player-1` or `player-2`."
  [board player FoW?]
  [map? keyword? boolean? :ret nil?]
  (let [max-range (apply max (map count (vals (player board))))
        counter (if (= :player-1 player) (range max-range) (range (dec max-range) -1 -1))]
    (doseq [index counter]
      (->> (if FoW? colors (conj colors :hand))
           (map #(let [value-print (value-for-print (get-in board [player % index :value]))]
                   (if (= % :hand)
                     (str (second (str (get-in board [player % index :suit]))) value-print)
                     value-print)))
           (interleave card-value-padding)
           (apply str)
           println))))

(>defn- print-discards!
  "Prints the discard-line."
  [board]
  [map? :ret nil?]
  (->> colors
       (map #(value-for-print (:value (peek (get-in board [:board %])))))
       (interleave card-value-padding)
       (apply str)
       println))

(>defn print-board!
  "Prints a board to the console. If `FoW?` is true a fog of war is activated that obscures the hand of :player-2."
  [board FoW?]
  [map? boolean? :ret nil?]
  (print-player! board :player-2 FoW?)
  (println "--------------------------")
  (println "  yel whi gre blu red hand")
  (print-discards! board)
  (println "--------------------------")
  (print-player! board :player-1 false)
  (println "###########################")
  (println "Left in Deck: " (count (get-in board [:board :draw-pile])))
  (println (format "Cards in hand: \n  Player 1: %s \n  Player 2: %s"
                   (count (get-in board [:player-1 :hand]))
                   (count (get-in board [:player-2 :hand])))))

(comment
  (print-board!
    {:player-1 {:yellow [{:suit :yellow :value 3} {:suit :yellow :value 5} {:suit :yellow :value 6}]
                :white [{:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value 3} {:suit :white :value 5} {:suit :white :value 10}]
                :green [{:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value 3}]
                :blue [{:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value 3}]
                :red [{:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value 3}]
                :hand [{:suit :yellow :value 5} {:suit :blue :value 10}]}
     :player-2 {:yellow [{:suit :yellow :value 3} {:suit :yellow :value 5} {:suit :yellow :value 6}]
                :white [{:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value 3} {:suit :white :value 5} {:suit :white :value 10}]
                :green [{:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value 3}]
                :blue [{:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value 3}]
                :red [{:suit :white :value :wager} {:suit :white :value :wager} {:suit :white :value 3}]
                :hand [{:suit :yellow :value 5} {:suit :yellow :value 6}]}
     :board {:yellow [{:suit :yellow :value 2} {:suit :yellow :value 10}]
             :white []
             :green []
             :blue []
             :red []
             :draw-pile []}}
    true)
  )