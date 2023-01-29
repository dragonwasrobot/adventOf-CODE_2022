;; Helpers

(defn slice [start length l]
  (->> l
     (drop start)
     (take length)))

(defn inspect [log value]
  (println log value)
  value)

;; Solution

(def sampleInput ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def shapes [:horizontalBar, :plus, :wedge, :verticalBar, :square])

(def startingPosition '(0 2))

(defn parseInput [useFile]
  (let [rawInput (if useFile (slurp "day17-input.txt") sampleInput)]
    (->> rawInput
       clojure.string/trim
       seq
       (map (fn [direction]
              (if (= \> direction)
                :right
                :left))))))

(defn height [shape]
  (case shape
    :horizontalBar 1
    :plus 3
    :wedge 3
    :verticalBar 4
    :square 2))

(defn adjustBoard [configuration]

  (let [shapeHeight (height (nth shapes (:shapeIdx configuration)))

        targetHeight (+ shapeHeight 3)

        board (:board configuration)

        rockIdx (first (keep-indexed (fn [idx row]
                                       (if (some (fn [col] (true? col)) row)
                                         idx
                                         nil)) board))

        newBoard (cond
                   (< rockIdx targetHeight) (vec (->> (range 0 (- targetHeight rockIdx))
                                                    (reduce (fn [boardAcc _]
                                                              (let [newRow (map (fn [_] false) (range 0 7))]
                                                                (vec (cons (vec newRow) boardAcc))))
                                                            board)))

                   (= rockIdx targetHeight) board

                   (> rockIdx targetHeight) (vec (drop (- rockIdx targetHeight) board)))

        initOutline [-1 -1 -1 -1 -1 -1 -1]

        outline (->> (range 0 (count newBoard))
                   (reduce (fn [accOutline rowIdx]
                             (let [newOutlineResult
                                   (->> (nth newBoard rowIdx)
                                      (reduce (fn [innerAcc c]
                                                (let [idx (first innerAcc)
                                                      acc (last innerAcc)]
                                                  (cond
                                                    (<= 0 (nth acc idx)) (list (inc idx) acc)
                                                    (= c true) (list (inc idx) (assoc acc idx rowIdx))
                                                    true (list (inc idx) acc))))
                                              (list 0 accOutline)))
                                   newOutline (last newOutlineResult)
                                   ]
                               (if (every? (fn [i] (<= 0 i)) newOutline)
                                 (reduced newOutline)
                                 newOutline)))
                           initOutline))

        newRockIdx (first (keep-indexed (fn [idx row]
                                          (if (some (fn [col] (true? col)) row)
                                            idx
                                            nil)) newBoard))
        newHeight (- (count newBoard) newRockIdx 1)
        newHistory (conj (:history configuration) (- newHeight (:height configuration)))]

    (assoc configuration :board newBoard
           :height newHeight
           :history newHistory)))

(defn createInitialConfiguration [directions]
  (let [rockBottom [(vec (->> (range 0 7)
                            (map (fn [_] true))))]

        initialBoard (vec (->> (range 0 3)
                             (reduce (fn [acc _]
                                       (let [emptyRow (map (fn [_] false) (range 0 7))]
                                         (cons (vec emptyRow) acc)))
                                     (vec rockBottom))))]

    {
     :board initialBoard
     :directions (vec directions)
     :directionIdx 0
     :shapeIdx 0
     :history []
     :rocks 1
     :height 0
     }))

(defn placeRockHorizontalBar [rowIdx colIdx board]
  (update board rowIdx
          (fn [row]
            (->> (range 0 4)
               (reduce (fn [rowAcc offset]
                         (assoc rowAcc (+ colIdx offset) true))
                       row)))))

(defn placeRockPlus [rowIdx colIdx board]
  (let [updatedFirstRow (update board rowIdx
                                (fn [row] (assoc row (+ colIdx 1) true)))

        updatedSecondRow (update updatedFirstRow (+ rowIdx 1)
                                 (fn [row]
                                   (->> (range 0 3)
                                      (reduce (fn [rowAcc offset]
                                                (assoc rowAcc (+ colIdx offset) true))
                                              row))))

        updatedThirdRow (update updatedSecondRow (+ rowIdx 2)
                                (fn [row] (assoc row (+ colIdx 1) true)))
        ]
    updatedThirdRow))

(defn placeRockWedge [rowIdx colIdx board]
  (let [updatedFirstRow (update board rowIdx
                                (fn [row] (assoc row (+ colIdx 2) true)))

        updatedSecondRow (update updatedFirstRow (+ rowIdx 1)
                                 (fn [row] (assoc row (+ colIdx 2) true)))

        updatedThirdRow (update updatedSecondRow (+ rowIdx 2)
                                (fn [row]
                                  (->> (range 0 3)
                                     (reduce (fn [rowAcc offset]
                                               (assoc rowAcc (+ colIdx offset) true))
                                             row))))]
    updatedThirdRow))

(defn placeRockVerticalBar [rowIdx colIdx board]
  (let [updatedFirstRow (update board rowIdx
                                (fn [row] (assoc row colIdx true)))

        updatedSecondRow (update updatedFirstRow (+ rowIdx 1)
                                 (fn [row] (assoc row colIdx true)))

        updatedThirdRow (update updatedSecondRow (+ rowIdx 2)
                                (fn [row] (assoc row colIdx true)))

        updatedFourthRow (update updatedThirdRow (+ rowIdx 3)
                                 (fn [row] (assoc row colIdx true)))]

    updatedFourthRow))

(defn placeRockSquare [rowIdx colIdx board]
  (let [updatedFirstRow (update board rowIdx
                                (fn [row]
                                  (->> (range 0 2)
                                     (reduce (fn [rowAcc offset]
                                               (assoc rowAcc (+ colIdx offset) true))
                                             row))))

        updatedSecondRow (update updatedFirstRow (inc rowIdx)
                                 (fn [row]
                                   (->> (range 0 2)
                                      (reduce (fn [rowAcc offset]
                                                (assoc rowAcc (+ colIdx offset) true))
                                              row))))]
    updatedSecondRow))

(defn placeRock [shape position board]
  (let [rowIdx (first position)
        colIdx (second position)]
    (case shape
      :horizontalBar (placeRockHorizontalBar rowIdx colIdx board)
      :plus (placeRockPlus rowIdx colIdx board)
      :wedge (placeRockWedge rowIdx colIdx board)
      :verticalBar (placeRockVerticalBar rowIdx colIdx board)
      :square (placeRockSquare rowIdx colIdx board))))

(defn applyDirection [position direction]

  (let [row (first position)
        col (second position)]
    (if (identical? direction :left)
      (list row (- col 1))
      (list row (+ col 1)))))

(defn applyGravity [position]
  (let [row (first position)
        col (second position)]
    (list (+ row 1) col)))

(defn canMoveHorizontalBar? [rowIdx colIdx board]
  (if (or (< colIdx 0) (<= 7 (+ colIdx 3)))
    false
    (->> (nth board rowIdx)
       (slice colIdx 4)
       (every? (fn [x] (not x))))))

(defn canMovePlus? [rowIdx colIdx board]
  (if (or (< colIdx 0) (<= 7 (+ colIdx 2)))
    false
    (let [row0 (nth board rowIdx)
          upperClear (not (nth row0 (+ colIdx 1)))

          row1 (nth board (+ rowIdx 1))
          middleClear (every? (fn [col] (not col)) (slice colIdx 3 row1))

          row2 (nth board (+ rowIdx 2))
          lowerClear (not (nth row2 (+ colIdx 1)))]

      (and upperClear middleClear lowerClear))))

(defn canMoveWedge? [rowIdx colIdx board]
  (if (or (< colIdx 0) (<= 7 (+ colIdx 2)))
    false
    (let [row0 (nth board rowIdx)
          upperClear (not (nth row0 (+ colIdx 2)))

          row1 (nth board (+ rowIdx 1))
          middleClear (not (nth row1 (+ colIdx 2)))

          row2 (nth board (+ rowIdx 2))
          lowerClear (every? (fn [col] (not col)) (slice colIdx 3 row2))]

      (and upperClear middleClear lowerClear))))

(defn canMoveVerticalBar? [rowIdx colIdx board]
  (if (or (< colIdx 0) (<= 7 colIdx))
    false
    (->> (range 0 4)
       (map (fn [idx] (nth (nth board (+ rowIdx idx)) colIdx)))
       (every? (fn [col] (not col))))))

(defn canMoveSquare? [rowIdx colIdx board]
  (if (or (< colIdx 0) (<= 7 (+ colIdx 1)))
    false
    (let [upperClear (->> (nth board rowIdx)
                        (slice colIdx 2)
                        (every? (fn [col] (not col))))

          lowerClear (->> (nth board (+ rowIdx 1))
                        (slice colIdx 2)
                        (every? (fn [col] (not col))))]
      (and upperClear lowerClear))))

(defn canMove? [position shape board]
  (let [rowIdx (first position)
        colIdx (second position)]
    (case shape
      :horizontalBar (canMoveHorizontalBar? rowIdx colIdx board)
      :plus (canMovePlus? rowIdx colIdx board)
      :wedge (canMoveWedge? rowIdx colIdx board)
      :verticalBar (canMoveVerticalBar? rowIdx colIdx board)
      :square (canMoveSquare? rowIdx colIdx board))))

(defn moveShape [position configuration]
  (let [shape (nth shapes (:shapeIdx configuration))

        ;; Air flow movement
        airflowDirection (nth (:directions configuration) (:directionIdx configuration))
        sidewaysPosition (applyDirection position airflowDirection)
        newPosition (if (canMove? sidewaysPosition shape (:board configuration))
                      sidewaysPosition
                      position)
        newDirectionIdx (if (>= (+ 1 (:directionIdx configuration))
                               (count (:directions configuration)))
                          0
                          (+ 1 (:directionIdx configuration)))

        ;; Gravity movement
        downPosition (applyGravity newPosition)
        ]

    (if (canMove? downPosition shape (:board configuration))
      ;; Inductive base
      (let [newConfiguration (assoc configuration :directionIdx newDirectionIdx)]
        (moveShape downPosition newConfiguration))
      ;; Base case
      (let [newRocks (+ 1 (:rocks configuration))
            newShapeIdx (if (>= (+ 1 (:shapeIdx configuration)) (count shapes))
                          0
                          (+ 1 (:shapeIdx configuration)))
            newBoard (placeRock shape newPosition (:board configuration))]

        (assoc configuration
               :board newBoard
               :directionIdx newDirectionIdx
               :shapeIdx newShapeIdx
               :rocks newRocks
               )))))

(defn calculatePart1 [useFile]
  (let [parsedInput (parseInput useFile)
        initialConfiguration (createInitialConfiguration parsedInput)
        rounds 2022

        finalConfiguration (->> (range 0 rounds)
                              (reduce (fn [accConfiguration round]
                                        (let [adjustedConfiguration (adjustBoard accConfiguration)
                                              newConfiguration (moveShape
                                                                startingPosition
                                                                adjustedConfiguration)]
                                          newConfiguration))
                                      initialConfiguration))

        finalRockIdx (first (keep-indexed (fn [idx row]
                                            (if (some (fn [col] (true? col)) row)
                                              idx
                                              nil)) (:board finalConfiguration)))

        finalHeight (- (count (:board finalConfiguration)) finalRockIdx 1)]
    ;; final height = 3219
    finalHeight))

(defn computeStartOfCycle [configuration]
  (let [history (:history configuration)
        historyStr (clojure.string/join "" history)
        candidateSize 1000
        candidateStr (subs historyStr (- (count historyStr) candidateSize) (count historyStr))
        refStr (subs historyStr 0 (- (count historyStr) candidateSize))
        cycleIdx (clojure.string/index-of refStr candidateStr)
        ]
    cycleIdx))

(defn detectCycle [initialConfiguration]
  (->> (range 0 3000)
     (reduce (fn [accConfiguration round]
               (let [adjustedConfiguration (adjustBoard accConfiguration)
                     newConfiguration (moveShape
                                       startingPosition
                                       adjustedConfiguration)]
                 (if (< round 1000)
                   newConfiguration
                   (let [cycleStartRound (computeStartOfCycle newConfiguration)
                         cycleStopRound (- round 999)]
                     (if (nil? cycleStartRound)
                       newConfiguration
                       (reduced (list cycleStartRound
                                      cycleStopRound
                                      newConfiguration)))))))
             initialConfiguration)))

(defn simulateRounds [rounds initialConfiguration]
  (->> (range 0 rounds)
     (reduce (fn [accConfiguration round]
               (let [adjustedConfiguration (adjustBoard accConfiguration)
                     newConfiguration (moveShape
                                       startingPosition
                                       adjustedConfiguration)]
                 newConfiguration))
             initialConfiguration)))

(defn calculatePart2 [useFile]
  (let [parsedInput (parseInput useFile)
        initialConfiguration (createInitialConfiguration parsedInput)

        haltedConfiguration (detectCycle initialConfiguration)

        cycleStartRound (nth haltedConfiguration 0)
        cycleStopRound (nth haltedConfiguration 1)
        finalConfiguration (nth haltedConfiguration 2)

        cycleStartHeight (:height (simulateRounds cycleStartRound initialConfiguration))
        cycleStopHeight (:height (simulateRounds cycleStopRound initialConfiguration))

        roundDiff (- cycleStopRound cycleStartRound)
        heightDiff (- cycleStopHeight cycleStartHeight)

        totalRocks 1000000000000
        numberOfCycles (double (/ (- totalRocks cycleStartRound) roundDiff))
        wholeCycles (int numberOfCycles)
        remainderCycle (- numberOfCycles wholeCycles)

        wholeCyclesHeight (* wholeCycles heightDiff)

        remainderRounds (+ cycleStartRound (int (* remainderCycle roundDiff)))
        remainderCycleHeight (:height (simulateRounds remainderRounds initialConfiguration))

        finalHeight (+ wholeCyclesHeight remainderCycleHeight 1)]
    ;; final height =  1_582_758_620_701
    finalHeight))

(defn run [useFile]
  (let [answer1 (calculatePart1 useFile)
        answer2 (calculatePart2 useFile)]
    (println "Answer 1:" answer1)
    (println "Answer 2:" answer2)))

(run true)
