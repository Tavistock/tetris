(ns tetris.game.board)

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

(defn get-drop-pos
  [board piece [x y]]
  (let [clear? #(piece-fits? board piece [x %])
        cy (first (remove clear? (iterate inc y)))]
    (max y (dec cy))))

(defn in-drop-pos?
  [board piece [x y]]
  (= (get-drop-pos board piece [x y]) y))

(defn get-rand-piece
  []
  (let [akey (rand-nth (keys coord-piece))]
        {:coord (coord-piece akey)
         :type akey}))

(defn any-filled-row?
  [board]
  (some true? (filled-rows board)))
    
(defn filled-rows
  "returns a list of wether a row is filled or not"
  [board]
  (map (partial not-any? zero?) board))

(defn collapse-rows
  "Returns a new board with the given row indices collapsed."
  [rows board]
  (let [cleared-board (->> board
                           (map-indexed vector)
                           (remove (fn [[i _]] (some true? (map #(= i %) rows))))
                           (map second))
        n (count rows)
        new-board (into (vec (repeat n empty-row)) cleared-board)]
    new-board))
