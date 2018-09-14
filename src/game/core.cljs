(ns game.core
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require
   [weasel.repl :as repl]
   [cljs.reader :refer [read-string]]
   [game.board :refer [piece-fits?
                       rotate-piece
                       start-position
                       empty-board
                       get-drop-pos
                       get-rand-piece
                       get-rand-diff-piece
                       write-piece-to-board
                       write-piece-behind-board
                       create-drawable-board
                       get-filled-row-indices
                       clear-rows
                       game-over-row
                       collapse-rows
                       highlight-rows
                       write-to-board
                       n-rows
                       n-cols
                       rows-cutoff
                       next-piece-board
                       tower-height]]
   [game.rules :refer [get-points
                       level-up?
                       grav-speed
                       initial-shift-speed
                       shift-speed]]
   [game.paint :refer [size-canvas!
                       cell-size
                       draw-board!]]
   [cljs.core.async :refer [close! put! chan <! timeout unique alts!]]))

(enable-console-print!)

(def $ js/jQuery)

(def search-type :bfs)
(def search-types #{:dfs :bfs})

(def heuristic-type :fill-wells)
(def heuristic-types #{:height :fill-wells :clear-lines})

;;------------------------------------------------------------
;; STATE OF THE GAME
;;------------------------------------------------------------

(def state
  "The state of the game."
  (atom nil))

(def solution-space
  "A copy of the game-space for the AI to manipulate"
  (atom nil))

(defn init-state!
  "Set the initial state of the game."
  []
  (reset! state {:next-piece nil
                 :piece nil
                 :position nil
                 :board empty-board
                 :score 0
                 :level 0
                 :level-lines 0
                 :total-lines 0
                 :soft-drop false
                 :running? true}))

(defn init-solution-space
  "Set the initial solution-space"
  [game-state]
  (reset! solution-space {:nodes [game-state]
                          :visited #{}
                          :best-node {}}))

(def paused? (atom false)) ;; Required for pausing/resuming the gravity routine

(declare go-go-gravity!)
(declare move-piece) ;; Required for recursive AI
(declare game-loop)  ;; Required to give control flow back to the game-loop

(def stop-grav (chan))
(defn stop-gravity! []
  (put! stop-grav 0))

(defn refresh-gravity! []
  (stop-gravity!)
  (go-go-gravity!))

;;------------------------------------------------------------------------------
;; Piece Control Throttling
;;------------------------------------------------------------------------------

;; These channels can received boolean signals indicating on/off status.
;; Duplicate signals are ignored with the (dedupe) transducer.

(def move-left-chan (chan 1 (dedupe)))
(def move-right-chan (chan 1 (dedupe)))
(def move-down-chan (chan 1 (dedupe)))

(defn go-go-control-soft-drop!
  "Monitor move-down-chan to update the gravity speed."
  []
  (go-loop []
    (swap! state assoc :soft-drop (<! move-down-chan))
    (refresh-gravity!)
    (recur)))

(declare try-move!)

(defn go-go-piece-shift!
  "Shifts a piece in the given direction until given channel is closed."
  [stop-chan dx]
  (go-loop [speed initial-shift-speed]
    (try-move! dx 0)
    (let [[value c] (alts! [stop-chan (timeout speed)])]
      (when-not (= c stop-chan)
        (recur shift-speed)))))

(defn go-go-control-piece-shift!
  "Monitors the given shift-chan to control piece-shifting."
  [shift-chan dx]
  (go-loop [stop-chan (chan)]
    (recur (if (<! shift-chan)
             (do (go-go-piece-shift! stop-chan dx)
                 stop-chan)
             (do (close! stop-chan)
                 (chan))))))

;;------------------------------------------------------------
;; STATE MONITOR
;;------------------------------------------------------------

(defn make-redraw-chan
  "Create a channel that receives a value everytime a redraw is requested."
  []
  (let [redraw-chan (chan)
        request-anim #(.requestAnimationFrame js/window %)]
    (letfn [(trigger-redraw []
              (put! redraw-chan 1)
              (request-anim trigger-redraw))]
      (request-anim trigger-redraw)
      redraw-chan)))

(defn drawable-board
  "Draw the current state of the board."
  []
  (let [{piece :piece
         [x y] :position
         board :board} @state]
    (create-drawable-board piece x y board)))

(defn go-go-draw!
  "Kicks off the drawing routine."
  []
  (let [redraw-chan (make-redraw-chan)]
    (go-loop [board nil]
      (<! redraw-chan)
      (let [new-board (drawable-board)
            next-piece (:next-piece @state)]
        (when (not= board new-board)
          (draw-board! "game-canvas" new-board cell-size rows-cutoff)
          (draw-board! "next-canvas" (next-piece-board next-piece) cell-size))
        (recur new-board)))))

;;------------------------------------------------------------
;; Game-driven STATE CHANGES
;;------------------------------------------------------------

(defn go-go-game-over!
  "Kicks off game over routine. (and get to the chopper)"
  []
  (go
    (doseq [y (reverse (range n-rows))]
      (<! (timeout 10))
      (swap! state assoc-in [:board y] (game-over-row))
      (swap! state assoc :running? false))))

(defn spawn-piece!
  "Spawns the given piece at the starting position."
  [piece]
  (swap! state assoc :piece piece
         :position start-position))

(defn try-spawn-piece!
  "Checks if new piece can be written to starting position."
  []
  (let [piece (or (:next-piece @state) (get-rand-piece))
        next-piece (get-rand-diff-piece piece)
        [x y] start-position
        board (:board @state)]

    (swap! state assoc :next-piece next-piece)

    (if (piece-fits? piece x y board)
      (spawn-piece! piece)
      (go ;; Exit-able
        ;; Show piece that we attempted to spawn, drawn behind the other pieces.
        ;; Then pause before kicking off gameover animation.
        (swap! state update-in [:board] #(write-piece-behind-board piece x y %))
        (<! (timeout (grav-speed (:level @state))))
        (go-go-game-over!)))))

(defn display-points!
  []
  (.html ($ "#score") (str "Score: " (:score @state)))
  (.html ($ "#level") (str "Level: " (:level @state)))
  (.html ($ "#lines") (str "Lines: " (:total-lines @state))))

(defn update-points!
  [rows-cleared]
  (let [n rows-cleared
        level (:level @state)
        points (get-points n (inc level))
        level-lines (+ n (:level-lines @state))]
    ;; update the score before a possible level-up
    (swap! state update-in [:score] + points)
    (if (level-up? level-lines)
      (do
        (swap! state update-in [:level] inc)
        (swap! state assoc :level-lines 0))
      (swap! state assoc :level-lines level-lines))
    (swap! state update-in [:total-lines] + n))

  (display-points!))

(defn collapse-rows!
  "Collapse the given row indices."
  [rows]
  (let [n (count rows)
        board (collapse-rows rows (:board @state))]
    (swap! state assoc :board board)
    (update-points! n)))

(defn go-go-collapse!
  "Starts the collapse animation if we need to, returning nil or the animation channel."
  []
  (let [board (:board @state)
        rows (get-filled-row-indices board)
        flashed-board (highlight-rows rows board)
        cleared-board (clear-rows rows board)]

    (when-not (zero? (count rows))
      (go
        ;; No need to exit this (just let it finish)
        ;; Blink n times
        (doseq [i (range 3)]
          (swap! state assoc :board flashed-board)
          (<! (timeout 170))
          (swap! state assoc :board board)
          (<! (timeout 170)))
        (swap! state assoc :board cleared-board) ;; Clear rows to create a gap, and pause
        (<! (timeout 220))
        (collapse-rows! rows))))) ;; Finally collapse

(defn lock-piece!
  "Lock the current piece into the board."
  []
  (let [[x y] (:position @state)
        piece (:piece @state)
        board (:board @state)
        new-board (write-piece-to-board piece x y board)]
    (swap! state assoc :board new-board
           :piece nil
           :soft-drop false) ;; Reset soft drop
    ;; If collapse routine returns a channel, wait for it before spawning a new piece.
    (if-let [collapse-anim (go-go-collapse!)]
      (go
        (<! collapse-anim)
        (<! (timeout 100))
        (try-spawn-piece!))
      (try-spawn-piece!))))

(defn apply-gravity!
  "Move current piece down 1 if possible, else lock the piece."
  []
  (let [piece (:piece @state)
        [x y] (:position @state)
        board (:board @state)
        ny (inc y)]
    (when (piece-fits? piece x ny board)
      (swap! state assoc-in [:position 1] ny))))

(defn go-go-gravity!
  "Starts the gravity routine."
  []
  (go-loop []
    (let [speed (grav-speed (:level @state) (:soft-drop @state))
          [_ c] (alts! [(timeout speed) stop-grav])]
      (when-not (= c stop-grav)
        (apply-gravity!)
        (recur)))))

;;------------------------------------------------------------
;; Input-driven STATE CHANGES
;;------------------------------------------------------------

(defn resume-game!
  "Restores the state of the board pre-pausing, and resumes gravity"
  []
  (go-go-gravity!)
  (reset! paused? false))

(defn pause-game!
  "Saves the current state of the board, loads the game-over animation and pauses gravity"
  []
  (stop-gravity!)
  (reset! paused? true))

(defn toggle-pause-game!
  "Toggles pause on the game board"
  []
  (if @paused?
    (resume-game!)
    (pause-game!)))

(defn try-move!
  "Try moving the current piece to the given offset."
  [dx dy]
  (let [[x y] (:position @state)
        piece (:piece @state)
        board (:board @state)
        nx (+ dx x)
        ny (+ dy y)]
    (if (piece-fits? piece nx ny board)
      (swap! state assoc :position [nx ny]))))

(defn try-rotate!
  "Try rotating the current piece."
  []
  (let [[x y] (:position @state)
        piece (:piece @state)
        board (:board @state)
        new-piece (rotate-piece piece)]
    (when (piece-fits? new-piece x y board)
      (swap! state assoc :piece new-piece))))

(defn hard-drop!
  "Hard drop the current piece."
  []
  (let [[x y] (:position @state)
        piece (:piece @state)
        board (:board @state)
        ny (get-drop-pos piece x y board)]
    (swap! state assoc :position [x ny])
    (lock-piece!)))

(def key-names {37 :left
                38 :up
                39 :right
                40 :down
                32 :space
                16 :shift
                80 :p})

(defn add-key-events
  "Add all the key inputs."
  []
  (let [key-name #(-> % .-keyCode key-names)
        key-down (fn [e]
                   (case (key-name e)
                     nil)
                   (when (:piece @state)
                     (case (key-name e)
                       :down  (put! move-down-chan true)
                       :left  (put! move-left-chan true)
                       :right (put! move-right-chan true)
                       :space (hard-drop!)
                       :up    (try-rotate!)
                       nil))
                   (when (#{:down :left :right :space :up} (key-name e))
                     (.preventDefault e)))
        key-up (fn [e]
                 (case (key-name e)
                   :down  (put! move-down-chan false)
                   :left  (put! move-left-chan false)
                   :right (put! move-right-chan false)
                   :p (toggle-pause-game!)
                   nil))]
    ;; Add key events
    (.addEventListener js/window "keydown" key-down)
    (.addEventListener js/window "keyup" key-up)))

;;------------------------------------------------------------
;; AI Stuff
;;------------------------------------------------------------

(defn ai-shift
  "Shifts a piece to the left or right in the AI game-board.
   This has no influence on the actual game-state"
  [dx dy sol-space]
  (let [[x y] (:position sol-space)
        piece (:piece sol-space)
        board (:board sol-space)
        nx (+ dx x)
        ny (+ dy y)]
    (if (piece-fits? piece nx ny board)
      (assoc sol-space :position [nx ny])
      {})))

(defn ai-move-down
  "Shifts a piece downwards in the AI game-board.
   This has no influence on the actual game-state"
  [sol-space]
  (let [piece (:piece sol-space)
        [x y] (:position sol-space)
        board (:board sol-space)
        ny (inc y)]
    (if (piece-fits? piece x ny board)
      (assoc sol-space :position [x ny])
      (assoc sol-space :locked? true))))

(defn ai-rotate
  "Rotates a piece in the AI game-board.
   This has no influence on the actual game-state"
  [sol-space]
  (let [[x y] (:position sol-space)
        piece (:piece sol-space)
        board (:board sol-space)
        new-piece (rotate-piece piece)]
    (if (piece-fits? new-piece x y board)
      (assoc sol-space :piece new-piece)
      {})))

(defn ai-lock-piece
  "Locks a piece into the AI game-board.
   This has no influence on the actual game-state"
  [game-state]
  (if (empty? game-state)
    game-state
    (let [[x y] (:position game-state)
          piece (:piece game-state)
          board (:board game-state)
          new-board (write-piece-to-board piece x y board)]
      (assoc game-state :board new-board))))

(defn get-new-nodes
  "Retreives neighbor states of a state. Possible state manipulations are: Shift Left,
  Shift Right, Shift Down, Rotate. Already visited states are excluded"
  [sol-space curr-node]
  (let [left-node (assoc (ai-shift -1 0 curr-node)
                         :solution (conj (:solution curr-node)
                                         :SL))
        right-node (assoc (ai-shift 1 0 curr-node)
                          :solution (conj (:solution curr-node)
                                          :SR))
        down-node (assoc (ai-move-down curr-node)
                         :solution (conj (:solution curr-node)
                                         :D))
        rotate-node (assoc (ai-rotate curr-node)
                           :solution (conj (:solution curr-node)
                                           :R))
        new-nodes (remove #(or (:locked? %) (empty? (:piece %))) [left-node right-node down-node rotate-node])
        node-list (case search-type
                    :bfs (apply conj (butlast (:nodes sol-space)) new-nodes)
                    :dfs (apply conj (rest (:nodes sol-space)) new-nodes))]
    ;;((fn [] (.log js/console (str node-list))))
    (remove #(contains? (:visited sol-space) {:position (:position %)
                                              :piece (:piece %)}) node-list)))

(defn get-score
  "Rates the quality of a node solution based on predefined heuristics.
   Lowest score is best."
  [node]
  (let [default-score 100]
    (if (empty? node)
      default-score
      (case heuristic-type
        :height (tower-height (:board node))
        :fill-wells (- default-score
                       (last (:position node)))
        :clear-lines (->> node :board
                          get-filled-row-indices
                          count
                          (- default-score))
        ;;:least-wells (->> )
        default-score))))

(defn check-best-sol
  "Compares the current solution to the best solution so far and accepts the better of the two
  Rating is determined by given heuristics defined in get-score"
  [best-node curr-node]
  ;;((fn [] (.log js/console (str "curr-node: "(get-score (ai-lock-piece curr-node)) " best-node " (get-score (ai-lock-piece best-node))))))
  (if (:locked? (ai-move-down curr-node))
    (if (< (get-score (ai-lock-piece best-node)) (get-score (ai-lock-piece curr-node)))
      best-node
      curr-node)
    best-node))

(defn stupid-dfs
  "Runs DFS to find the opimal location for :piece"
  [sol-space]
  (let [curr-node (first (:nodes sol-space))]
    ;;((fn [] (.log js/console (str "Nodes: " (count (:nodes sol-space)) " Visited: " (count (:visited sol-space))))))
    (cond
      (empty? curr-node) (:solution (:best-node sol-space))
      :else (let [new-nodes (get-new-nodes sol-space curr-node)
                  best-node (check-best-sol (:best-node sol-space) curr-node)]
              (stupid-dfs (assoc sol-space
                                 :nodes new-nodes
                                 :best-node best-node
                                 :visited (conj (:visited sol-space)
                                                {:position (:position curr-node)
                                                 :piece (:piece curr-node)})))))))

(defn stupid-bfs
  "Runs BFS and determines the optimal location for :piece"
  [sol-space]
  (let [curr-node (last (:nodes sol-space))]
    ;;((fn [] (.log js/console (str "Nodes: " (count (:nodes sol-space)) " Visited: " (count (:visited sol-space))))))
    (cond
      (empty? curr-node) (:solution (:best-node sol-space))
      :else (let [new-nodes (get-new-nodes sol-space curr-node)
                  best-node (check-best-sol (:best-node sol-space) curr-node)]
              (stupid-bfs (assoc sol-space
                                 :nodes new-nodes
                                 :best-node best-node
                                 :visited (conj (:visited sol-space)
                                                {:position (:position curr-node)
                                                 :piece (:piece curr-node)})))))))

(defn smart-dfs
  "Runs DFS twice to determine the optimal location for :piece and :next-piece"
  [sol-space]
  )

(defn smart-bfs
  "Runs BFS twice to determine the optimal location for :piece and :next-piece"
  [sol-space]
  )

(defn find-solution
  "Find a solution for a given game-state"
  [game-state]
  (init-solution-space game-state)
  (case search-type
    :dfs (stupid-dfs @solution-space)
    :bfs (stupid-bfs @solution-space)
    nil ((fn [] (.log js/console "Error")))))

;;------------------------------------------------------------
;; Game Loop
;;------------------------------------------------------------

(defn update-game
  "Locks the piece into the game board when the solution has finished executing"
  [sol]
  ;; Figure out way to update game between moves
  (if-not (empty? sol)
    (move-piece sol)
    (lock-piece!)))

(defn move-piece
  "Moves the game piece as specified by the given solution"
  [sol]
  ;; ((fn [] (.log js/console (str sol))))
  (if-not (empty? sol)
    (do (case (last sol)
          :SL (try-move! -1 0)
          :SR (try-move! 1 0)
          :D (apply-gravity!)
          :R (try-rotate!)
          nil)
        ;; (when (:running? @state)
        ;;   (js/setTimeout (update-game (rest sol)) 1000))
        (update-game (butlast sol)))
    (go-go-game-over!)))

(defn update-state
  "Repeatedly finds the solution for a given state and modifies the state accordingly"
  []
  (->> @state
       find-solution
       move-piece))

(defn game-loop
  "Main Loop"
  []
  ;; ((fn [] (.log js/console "Loop")))
  (when (:piece @state)
    (update-state))
  (when (:running? @state)
    (js/setTimeout game-loop 100)))

;;------------------------------------------------------------
;; Entry Point
;;------------------------------------------------------------

(defn init []
  ;; (repl/connect "ws://localhost:9001")
  (assert (search-type search-types)
          (str "Search Type " search-type " does not exist."))
  (assert (heuristic-type heuristic-types)
          (str "Heuristic Type " heuristic-type " does not exist."))
  (init-state!)
                                        ; (go-go-control-soft-drop!)
                                        ; (go-go-control-piece-shift! move-left-chan -1)
                                        ; (go-go-control-piece-shift! move-right-chan 1)
  (size-canvas! "game-canvas" empty-board cell-size rows-cutoff)
  (size-canvas! "next-canvas" (next-piece-board) cell-size)
  (try-spawn-piece!)
  ;; (add-key-events)
  (go-go-draw!)
  (display-points!)
  (game-loop))

($ init)
