(ns tetris.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [close! put! chan <! timeout unique alts!]]
            [tetris.game.board :as board :refer [any-filled-row?
                                                 filled-rows
                                                 get-rand-piece
                                                 get-drop-pos
                                                 rotate-piece
                                                 empty-board
                                                 piece-fits?
                                                 coord-piece
                                                 empty-row
                                                 collapse-rows]]
            [tetris.game.state :refer [game-state]]
            [tetris.game.paint :refer [tetris-board value-piece write-piece highlight-rows]]
            )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]))

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
  (when-let [piece (:coord (:piece @game-state))]
    (let [[x y] (:position @game-state)
          board (:board @game-state)
          nx    (+ dx x)
          ny    (+ dy y)]
      (if (piece-fits? board piece [nx ny])
        (swap! game-state assoc :position [nx ny])))))

(defn try-rotate!
  []
  (when-let [piece (:coord (:piece @game-state))]
    (let [pos       (:position @game-state)
          board     (:board @game-state)
          new-piece (rotate-piece piece)]
      (if (piece-fits? board new-piece pos)
        (swap! game-state assoc-in [:piece :coord] new-piece)))))

(defn do-drop!
  []
  (let [[x y] (:position @game-state)
        board (:board @game-state)
        piece (:piece @game-state)
        ny    (get-drop-pos board (:coord piece) [x y])]
    (swap! game-state assoc :position [x ny])))

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
                     :space (do-drop!)
                     :up    (try-rotate!)
                     nil)
                   (when (#{:down :left :right :space :up} (key-name e))
                     (.preventDefault e)))
        key-up (fn [e]
                 (case (key-name e)
                   :down  (put! move-down-chan false)
                   :left  (put! move-left-chan false)
                   :right (put! move-right-chan false)
                   ;:shift (toggle-record!)
                   nil))]
    
    ; Add key events
    (.addEventListener js/window "keydown" key-down)
    (.addEventListener js/window "keyup" key-up)))

; ::::::::::::::::::::::::::::::::::::::::::::::
; Game Controlled State Change
; ::::::::::::::::::::::::::::::::::::::::::::::

(def stop-grav (chan))
(defn stop-gravity! [] (put! stop-grav 0))

(declare go-gravity!)

(defn remove-rows! []
  (let [board (:board @game-state)
        rows (->> board
                  (filled-rows)
                  (map-indexed vector)
                  (filter #(true? (second %)))
                  (map first))
        collapsed   (collapse-rows rows board)
        highlighted (highlight-rows rows board)]
    (go ; no need to exit this (just let it finish)
        ; blink n times
        (doseq [i (range 3)]
          (swap! game-state assoc :board highlighted)
          (<! (timeout 170))
          (swap! game-state assoc :board board)
          (<! (timeout 170)))
        
        ; clear rows to create a gap, and pause
        (swap! game-state assoc :board highlighted)
        (<! (timeout 220))
        
        ; finally collapse
        (swap! game-state assoc :board collapsed)))
  (try-new-piece!))

(defn try-new-piece!
  []
  (let [piece (get-rand-piece)
        start-pos [4 1]
        board (:board @game-state)]
    (if (piece-fits? board (:coord piece) start-pos)
      (do (swap! game-state assoc
                 :piece (:next @game-state)
                 :next piece
                 :position start-pos)
        (go-gravity!))
      (js/console.log "shit")))); game-over

(defn lock-piece!
  []
  (let [piece (:piece @game-state)
        pos (:position @game-state)
        board (:board @game-state)
        new-board (write-piece board (:coord piece) pos (value-piece (:type piece)))]
    (swap! game-state assoc :board new-board)
    (stop-gravity!)
    (if (any-filled-row? new-board);  add conditions for line clear
      (remove-rows!)
      (try-new-piece!))))

(defn apply-gravity!
  []
  (let [coord (get-in @game-state [:piece :coord])
        [x y] (:position @game-state)
        board (:board @game-state)
        ny (inc y)]
    (if (piece-fits? board coord [x ny])
      (swap! game-state assoc-in [:position 1] ny)
      (lock-piece!))))

(defn go-gravity!
  []
  (go-loop []
           (let [speed 400
                 [_ c] (alts! [(timeout speed) stop-grav])]
             (when-not (= c stop-grav)
               (apply-gravity!)
               (recur)))))


; ::::::::::::::::::::::::::::::::::::::::::::::
; Om Rendering
; ::::::::::::::::::::::::::::::::::::::::::::::

(defn main []
  (om/root
    tetris-board
    game-state
    {:target (. js/document (getElementById "app"))}))

; (defn tetris-board [data owner]
;       (reify
;         om/IRender
;         (render [_]
;                 (apply dom/ul
;                        nil 
;                        (om/build-all row (create-drawable-board 
;                                            (:board data)
;                                            (:piece data)
;                                            (:position data)))))))

; (defn row [data owner]
;   (reify
;     om/IRender
;     (render [_]
;             (dom/li
;               nil 
;               (clj->js data)))))

(add-key-events)
(go-go-control-piece-shift! move-left-chan -1)
(go-go-control-piece-shift! move-right-chan 1)