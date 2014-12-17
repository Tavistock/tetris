(ns tetris.game.state
  (:require [tetris.game.board :refer [empty-board get-rand-piece]]))

(defonce game-state (atom {:board empty-board
                          :piece (get-rand-piece)
                          :next (get-rand-piece)
                          :position [4 1]}))