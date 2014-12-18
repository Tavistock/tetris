(ns tetris.game.paint
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [tetris.game.board :as board :refer [rows cols get-drop-pos]]))

(def cell-size (quot 600 rows))
(def dark "#449")
(def light "#6ad")
(def bg "#444")

(def value-piece
  "An ordering imposed on the possible cell types, used for tilemap position."
  { 0 0
   :I 1
   :L 2
   :J 3
   :S 4
   :Z 5
   :O 6
   :T 7
   :G 8  ; ghost piece
   :H 9  ; highlighted (filled or about to collapse)
   })

(defn default-theme [type]
  (case type
    8 ["#555" "#666"]  ; ghost piece
    9 ["#600" "#700"]
    [dark light]))

(defn write-piece
  [board coords [cx cy] value]
  (if-let [[x y] (first coords)]
    (recur (assoc-in board [(+ y cy) (+ x cx)] value)
           (rest coords)
           [cx cy]
           value)
    board))

(defn create-drawable-board
  [board piece [x y]]
  (let [gy     (get-drop-pos board  (:coord piece) [x y])
        board1 (write-piece  board  (:coord piece) [x gy] (value-piece :G))
        board2 (write-piece  board1 (:coord piece) [x y]  (value-piece (:type piece)))]
    board2))

(defn unroll-board
  [board]
  (for [[y row]   (map-indexed vector board)
        [x type] (map-indexed vector row)]
    [x y type]))


(defn change-rows
  [rows board type]
  (->> board
       (map-indexed vector)
       (map
         (fn [[i x]] (if (some true? (map #(= i %) rows))
                       (vec (repeat cols (value-piece type)))
                       x)))
       vec))

(defn highlight-rows [rows board] (change-rows rows board :H))

(defn clear-rows [rows board] (change-rows rows board 0))

(defn draw-cell!
  ([ctx [x y type]] (draw-cell! ctx [x y type] default-theme))
  ([ctx [x y type] theme]
   (when-not (= type 0)
     (let [[dark light] (theme type)]
       (set! (.. ctx -fillStyle) dark)
       (set! (.. ctx -strokeStyle) light)
       (set! (.. ctx -lineWidth) 2)
       (let [rx (* cell-size x)
             ry (* cell-size y)
             rs cell-size]
         (.. ctx (fillRect rx ry rs rs))
         (.. ctx (strokeRect rx ry rs rs)))))))

(defn draw-board!
  [canvas state]
  (let [ctx (.. canvas (getContext "2d"))
        rs cell-size]
    (set! (.. ctx -fillStyle) bg)
    (.. ctx (fillRect 0 0 (* cols rs) (* rows rs)))
    (doseq [:let [board (create-drawable-board (:board state)
                                               (:piece state)
                                               (:position state))]
            coords (unroll-board board)]
      (draw-cell! ctx coords))))