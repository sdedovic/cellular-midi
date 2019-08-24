(ns cellular-midi.core
  (:require [midi :as m])
  (:require [clojure.set :as set]))

(def under-population 1)
(def over-population 4)
(def parent-count 3)

(def box #{[2 1] [2 2] [1 1] [1 2]})
(def boat #{[1 1] [2 1] [1 2] [3 2] [2 3]})
(def blinker #{[3 1] [3 2] [3 3]})
(def glider #{[2 0] [0 1] [2 1] [1 2] [2 2]})
(def gosper-glider-gun #{[1 5] [2 5] [1 6] [2 6]
                         [11 5] [11 6] [11 7] [12 4] [12 8] [13 3] [13 9] [14 3] [14 9]
                         [15 6] [16 4] [16 8] [17 5] [17 6] [17 7] [18 6]
                         [21 3] [21 4] [21 5] [22 3] [22 4] [22 5] [23 2] [23 6] [25 1] [25 2] [25 6] [25 7]
                         [35 3] [35 4] [36 3] [36 4]})

;([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1])
(def neighbour-offsets
  (let [digits (range -1 2)]
    (for [x digits y digits
          :let [value [x y]]
          :when (not (= value [0 0])) ] value)))

(defn all-neighbours
  [cell]
  (for [offset neighbour-offsets] (map + offset cell)))

(defn alive-neighbours
  [cells cell]
  (filter #(contains? cells %) (all-neighbours cell)))

(defn dead-neighbours
  [cells cell]
  (filter #(not( contains? cells %)) (all-neighbours cell)))

(defn regulate
  [cells]
  (filter #(let [alive-neighbour-count (count (alive-neighbours cells %))]
             (and
               ;(>= 0 (first %))
               (> alive-neighbour-count under-population )
               (<  alive-neighbour-count over-population) ))cells))

(defn dead-neighbour-cells
  [cells]
  (reduce set/union (for [cell cells]  (dead-neighbours cells cell))))

(defn reproduce
  [cells]
  (filter #(= parent-count (count (alive-neighbours cells %))) (dead-neighbour-cells cells)))

(defn tick [cells]
  (set/select
    (fn [[x y]] (and (>= x 0)
                     (>= y 0)
                     (< x 8)
                     (< y 8)))
    (set/union (set (reproduce cells)) (set (regulate cells)))))

(def all-cells [0 1 2 3 4 5 6 7 16 17 18 19 20 21 22 23 32 33 34 35 36 37 38 39 48 49 50 51 52 53 54 55 64 65 66 67 68 69 70 71 80 81 82 83 84 85 86 87 96 97 98 99 100 101 102 103 112 113 114 115 116 117 118 119])

(defn clear-all [sink]
  (doseq [n all-cells]
    (m/midi-note-off sink n)))

(defn cell-to-note [[x y]]
  (let [note-val (+ x (* y 16))]
    note-val))

(defn note-to-cell [n]
  (let [x (mod n 16)
        y (int (/ n 16))]
    [x y]))

(defn draw-cells 
  ([sink cells]
    (draw-cells sink cells 127))
  ([sink cells vel]
    (doseq [note all-cells] (m/midi-note-off sink note))
    (doseq [cell cells] (m/midi-note-on sink (cell-to-note cell) vel))))

(defn draw-groups-init [sink]
  (m/midi-note-on sink (cell-to-note [8 0]) 16))

(defn group-to-vel
  ([group]
    (group-to-vel group :low))
  ([group amt]
    (cond
      (and (= group 0) (= amt :low))
      16
      
      (and (= group 0) (= amt :high))
      32
      
      :else 0)))

(def lp-out (m/midi-out))
(def lp-in (m/midi-in))

(def state (atom {:board #{[2 2] [3 2] [4 2]}
                  :groups [#{} #{} #{} #{} #{} #{} #{} #{}]
                  :render :board}))


(defn handle-event [state event]
  (let [board (:board state)
        render (:render state)
        {note :note cmd :cmd vel :vel} event]
    (cond 
      ;; cell
      (and (= 0 vel) (= 144 cmd) (some #(= note %) all-cells))
      (cond 
        (= render :board)
        (update state :board conj (note-to-cell note))

        (= render :group)
        (let [cell (note-to-cell note)
              group (:group state)
              cells (nth (:groups state) group)]
          (if (set/subset? [cell] cells)
            (do
              (m/midi-note-off lp-out note)
              (update-in state [:groups group] (partial set/select #(not (= note (cell-to-note %))))))
            (do
              (m/midi-note-on lp-out note (group-to-vel group))
              (update-in state [:groups group] conj cell)))))

      ;; group 1
      (and (= 0 vel) (= 144 cmd) (= 8 note))
      (do
        (m/midi-note-on lp-out (cell-to-note [8 0]) 32)
        (-> state
          (assoc :render :group)
          (assoc :group 0)))

      ;; top left
      (and (= 0 vel) (= 104 note) (= 176 cmd))
      (do
        (draw-groups-init lp-out)
        (assoc state :render :board))
        
      (= 0 vel)
      (do (println "note: " note " cmd: " cmd)
          state)
      
      :else state)))

(defn update-state [state]
  (let [board-notes (set (map cell-to-note (:board state)))
        groups-notes (map #(set (map cell-to-note %)) (:groups state))]
    (doseq [[idx cells] (map-indexed vector groups-notes)]
      (let [hits (set/intersection board-notes cells)
            any? (> (count hits) 0)]
        (println any?)
        (if any?
          (m/midi-note-on lp-out (cell-to-note [8 idx]) (group-to-vel idx :high))
          (m/midi-note-on lp-out (cell-to-note [8 idx]) (group-to-vel idx :low))))))
  (update state :board tick))

(defn draw [state]
  (let [render (:render state)]
    (cond
      (= render :group)
      (let [group (:group state)
            cells (nth (:groups state) group)
            vel (group-to-vel group)]
        (draw-cells lp-out cells vel))

      (= render :board)
      (draw-cells lp-out (:board state)))))

;; execute event handler on midi events
(m/midi-handle-events 
  lp-in 
  (fn [event _] (swap! state handle-event event)))

(draw @state)
(loop []
  (let [new-state (swap! state update-state)]
    (draw new-state)
    (Thread/sleep 350)
    (recur)))
