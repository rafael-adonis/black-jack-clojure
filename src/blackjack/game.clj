(ns blackjack.game
  (:require [card-ascii-art.core :as card]))

; General rules:
; J, Q, K = 10 points
; A = 1 or A = 11 | if total points is greater than 21, A equals 1
;                 | if total points is less than 21, A equals 11
; players win when have 21 points, loose when have more than 21 points

(def max-card-value 13)
(def max-points-game 21)

(defn new-card
  "Generate a new card between  1 and 13"
  []
  (inc (rand-int max-card-value)))

(defn JQK->10
  "Switch J, Q, K to 10 points"
  [card]
  (if (> card 10) 10 card))

(defn A->11
  [card]
  (if (= card 1) 11 card))

(defn total-points
  "Return a total points in cards"
  [cards]
  (let [cards-without-JQK (map JQK->10 cards)
        cards-with-aces-eleven (map A->11 cards-without-JQK)
        points-with-aces-one (reduce + cards-without-JQK)
        points-with-aces-eleven (reduce + cards-with-aces-eleven)]
    (if (> points-with-aces-eleven max-points-game)
      points-with-aces-one
      points-with-aces-eleven)))

(defn new-player
  "Define new player with two random cards"
  [player-name]
  (let [card-1 (new-card)
        card-2 (new-card)
        cards-on-hand [card-1 card-2]
        points (total-points cards-on-hand)]
    {:player-name player-name
     :cards       cards-on-hand
     :points      points}))

(defn another-card
  [player]
  (let [card (new-card)
        cards (conj (:cards player) card)
        ;new-player (assoc player :cards cards)
        new-player (update player :cards conj card)
        points (total-points cards)]
    (assoc new-player :points points)))

(defn player-decision-continue?
  [player]
  (println (:player-name player) ": mais carta?")
  (= (read-line) "sim"))

(defn dealer-decision-continue?
  [player-points dealer]
  (let [dealer-points (:points dealer)]
    (if (> player-points max-points-game)
      false
      (<= dealer-points player-points))))

(defn game
  [player fn-decision-continue?]
  (if (fn-decision-continue? player)
    (let [player-with-more-cards (another-card player)]
      (card/print-player player-with-more-cards)
      (recur player-with-more-cards fn-decision-continue?))
    player))

(defn end-game
  [player dealer]
  (let [player-points (:points player)
        dealer-points (:points dealer)
        player-name (:player-name player)
        dealer-name (:player-name dealer)
        message (cond
                  (and (> player-points max-points-game) (> dealer max-points-game)) "Ambos perderam"
                  (= player-points dealer-points) "Empatou"
                  (> player-points max-points-game) (str dealer-name " ganhou")
                  (> dealer-points max-points-game) (str player-name " ganhou")
                  (> player-points dealer-points) (str player-name " ganhou")
                  (> dealer-points player-points) (str dealer-name " ganhou"))]
    (card/print-player player)
    (card/print-player player)
    (println message)))

(def player (new-player "Rafael"))
(card/print-player player)

(def dealer (new-player "Dealer"))
(card/print-masked-player dealer)

(def player-after-game (game player player-decision-continue?))
(def partial-dealer-decision-continue? (partial dealer-decision-continue? (:points player-after-game)))
(def dealer-after-game (game dealer partial-dealer-decision-continue?))

(end-game player-after-game dealer-after-game)
