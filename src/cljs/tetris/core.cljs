(ns tetris.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [close! put! chan <! timeout unique alts!]]
            [tetris.game.board :as board])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))

(def coord-piece
  {:I [ [-1  0] [ 0  0] [ 1  0] [ 2  0] ]
   :T [ [ 0 -1] [-1  0] [ 0  0] [ 1  0] ]
   :O [ [ 0 -1] [ 1 -1] [ 0  0] [ 1  0] ]
   :J [ [-1 -1] [-1  0] [ 0  0] [ 1  0] ]
   :L [ [ 1 -1] [-1  0] [ 0  0] [ 1  0] ]
   :S [ [ 0 -1] [ 1 -1] [-1  0] [ 0  0] ]
   :Z [ [-1 -1] [ 0 -1] [ 0  0] [ 1  0] ]})

(defonce app-state (atom {:board empty-board
                          :piece {:coord (coord-piece :J) :type :J}
                          :position [4 6]}))

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

(defn rotate-coord [[x y]]
  [ (- y) x ])

(defn rotate-piece [piece]
  (mapv rotate-coord piece))

(defn piece-fits?
  [board piece [cx cy]]
  (every?
    (fn [[x y]]
      (zero? (get-in board [(+ y cy) (+ x cx)])))
    piece))

(defn write-piece
  [board coords [cx cy] value]
  (if-let [[x y] (first coords)]
    (recur (assoc-in board [(+ y cy) (+ x cx)] value)
           (rest coords)
           [cx cy]
           value)
    board))

(defn get-drop-pos
  [board piece [x y]]
  (let [clear? #(piece-fits? board piece [x %])
        cy (first (remove clear? (iterate inc y)))]
    (max y (dec cy))))

(defn create-drawable-board
  [board piece [x y]]
  (let [gy     (get-drop-pos board  (:coord piece) [x y])
        board1 (write-piece  board  (:coord piece) [x gy] "G")
        board2 (write-piece  board1 (:coord piece) [x y] (value-piece (:type piece)))]
    board2))

; ::::::::::::::::::::::::::::::::::::::::::::::
; Piece Control
; ::::::::::::::::::::::::::::::::::::::::::::::

(def move-left-chan (chan 1 (dedupe)))
(def move-right-chan (chan 1 (dedupe)))
(def move-down-chan (chan 1 (dedupe)))

(declare try-move!)

(defn go-go-piece-shift!
  "Shifts a piece in the given direction until given channel is closed."
  [stop-chan dx]
  (go-loop []
    (try-move! dx 0)
    (let [time- (timeout 300)
          [value c] (alts! [stop-chan time-])]
      (when (= c time-)
        (recur)))))

(defn go-go-control-piece-shift!
  "Monitors the given shift-chan to control piece-shifting."
  [shift-chan dx]
  (go-loop [stop-chan (chan)]
           (let [value (<! shift-chan)]
             (recur (if value
                      (do (go-go-piece-shift! stop-chan dx)
                        stop-chan)
                      (do (close! stop-chan)
                        (chan)))))))

;;------------------------------------------------------------
;; Input-driven STATE CHANGES
;;------------------------------------------------------------

(defn try-move!
  "Try moving the current piece to the given offset."
  [dx dy]
  (when-let [piece (:coord (:piece @app-state))]
    (let [[x y] (:position @app-state)
          board (:board @app-state)
          nx (+ dx x)
          ny (+ dy y)]
      (if (piece-fits? board piece [nx ny])
        (swap! app-state assoc :position [nx ny])))))

(defn try-rotate!
  []
  (when-let [piece (:coord (:piece @app-state))]
    (let [pos (:position @app-state)
          board (:board @app-state)
          new-piece (rotate-piece piece)]
      (if (piece-fits? board new-piece pos)
        (swap! app-state assoc-in [:piece :coord] new-piece)))))

; ::::::::::::::::::::::::::::::::::::::::::::::
; Controls
; ::::::::::::::::::::::::::::::::::::::::::::::

(def key-names {
                ; arrow
                ; letter (wasd)
                37 :left
                65 :left 
                38 :up
                87 :up
                39 :right
                68 :right
                40 :down
                83 :down
                
                32 :space
                16 :shift})

(defn add-key-events
  "Add all the key inputs."
  []
  (let [down-chan (chan)
        key-name #(-> % .-keyCode key-names)
        key-down (fn [e]
                   (case (key-name e)
                     :down  (put! move-down-chan true)
                     :left  (put! move-left-chan true)
                     :right (put! move-right-chan true)
                     ; :space (hard-drop!)
                     :up    (try-rotate!)
                     nil)
                   (when (#{:down :left :right :space :up} (key-name e))
                     (.preventDefault e)))
        key-up (fn [e]
                 (case (key-name e)
                   :down  (put! move-down-chan false)
                   :left  (put! move-left-chan false)
                   :right (put! move-right-chan false)
                   ; :space (hard-drop!)
                   ;:shift (toggle-record!)
                   nil))]
    
    ; Add key events
    (.addEventListener js/window "keydown" key-down)
    (.addEventListener js/window "keyup" key-up)))

; ::::::::::::::::::::::::::::::::::::::::::::::
; Om Rendering
; ::::::::::::::::::::::::::::::::::::::::::::::
(defn main []
  (om/root
    tetris-board
    app-state
    {:target (. js/document (getElementById "app"))}))

(defn tetris-board [app owner]
      (reify
        om/IRender
        (render [_]
                (apply dom/ul
                       nil 
                       (om/build-all row (create-drawable-board (:board app) (:piece app) (:position app)))))))

(defn row [data owner]
  (reify
    om/IRender
    (render [_]
            (dom/li
              nil 
              (clj->js data)))))

(add-key-events)
(go-go-control-piece-shift! move-left-chan -1)
(go-go-control-piece-shift! move-right-chan 1)