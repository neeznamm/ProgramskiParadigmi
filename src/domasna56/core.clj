(ns domasna56.core
  (:gen-class)
  (:require [quil.core :as q])
)

(require '[clojure.set])

(defn submatrix-with-index
  "Ја враќа подматрицата со индекс (i,j) од 9-те 3x3 подматрици на судокуто"
  [sudoku i j]
  (let [start-row (* i 3)
        start-col (* j 3)]
    (for [row (range start-row (+ start-row 3))]
      (subvec (get-in sudoku [row]) start-col (+ start-col 3))))
)

(defn submatrix-for-element
  "Ја враќа 3x3 подматрицата во која се наоѓа елементот со дадениот индекс"
  [sudoku i j]
  (submatrix-with-index sudoku (quot i 3) (quot j 3))
)

(defn get-column 
  "Ја враќа колоната со индекс col-index од судокуто"
  [sudoku col-index]
  (mapv #(nth % col-index) sudoku)
)

(defn find-diff
  "Ја наоѓа разликата на starting-set и S_Row U S_Col U S_Submatrix, каде S_Row, S_Col и S_Submatrix се множествата од елементи во истата редица, колона и подматрица на елементот со индекс (i,j) во matrix соодветно."
  [starting-set matrix i j]
   (clojure.set/difference
                     starting-set
                     (apply clojure.set/union
                            (map set [;; брови во истата редица
                                      (get-in matrix [i])
                                      ;; броеви во истата колона
                                      (get-column matrix j)
                                      ;; броеви во истата подматрица (прво израмнета со apply concat)
                                      (apply concat (submatrix-for-element matrix i j))])))
)

(defn transform
  "Враќа матрица каде елементот со индекс (i,j) во судокуто е заменет со множество од можни вредности за таа позиција."
  [sudoku]
  (let [rows (count sudoku)
        cols (count (first sudoku))]
    (vec (for [i (range rows)]
           (vec (for [j (range cols)]
                  (if (zero? (get-in sudoku [i j]))
                   #{1 2 3 4 5 6 7 8 9}
                   #{(get-in sudoku [i j])})                 
                 )
           )
         )
    )
  )
)

(defn singleton-set?
  "Враќа true ако s е множество со само еден елемент"
  [s]
  (= 1 (count s))
)

(defn in-same-submatrix?
  "Враќа true ако елементот со индекс (i,j) и тој со индекс (x,y) се во истата 3x3 подматрица во судокуто"
  [i j x y]
  (and (= (quot i 3) (quot x 3)) (= (quot j 3) (quot y 3)))
)

(defn rm-number-from-same-row-col-submatrix
  "Прима матрица каде елементите се множества од можни вредности и враќа нова матрица каде number е избришан од множествата на ќелиите со иста редица, колона или подматрица како таа со индекс (i,j)"
  [matrix i j number]
  (let [rows (count matrix)
        cols (count (first matrix))]
    (vec (for [x (range rows)]
           (vec (for [y (range cols)]
                  (let [current-set (get-in matrix [x y])]
                    (if (and (or (= x i) (= y j) (in-same-submatrix? i j x y)) (not (singleton-set? current-set)))
                      (set (remove #(= number %) current-set))
                      current-set)))))))
)

(defn process-element
  "Ова се чекорите 1 и 2 од барањето споени во едно т.е. условно се применуваат, зависно од тоа дали елементот е едночлено множество или множество што содржи уникатен елемент (таков што не се појавува во истата колона, редица или подматрица). Ако елементот не исполнува ниту еден од условите, матрицата не се менува."
  [matrix i j]
  (if (singleton-set? (get-in matrix [i j]))
    ;; Ако елементот е множество со еден број, бриши го тој број од множествата на елементите во иста редица, колона и подматрица
    (rm-number-from-same-row-col-submatrix matrix i j (first (get-in matrix [i j])))
    ;; Во спротивно, види дали има број во множеството на елементот што не се појавува во множ. на елементите од иста редица, колона и подматрица. Ако има, постави го елементот на едночлено множество (што го содржи само соодветниот број)
    (let [current-set (get-in matrix [i j])
          row-set (apply clojure.set/union (assoc (get-in matrix [i]) j nil))
          col-set (apply clojure.set/union (assoc (get-column matrix j) i nil))
          submatrix-set (apply clojure.set/union (assoc (vec (apply concat (submatrix-for-element matrix i j))) (+ (* 3 (mod i 3)) (mod j 3)) nil))
          diff (clojure.set/difference current-set (clojure.set/union row-set col-set submatrix-set))]
      (if (empty? diff) matrix (assoc-in matrix [i j] (into #{} (list (first diff))))))))

(defn process-all-elements
 "Едно поминување на елементите на матрицата со process-element"
  [matrix]
  (loop [matrix matrix
         i 0
         j 0]
    (if (< i (count matrix))
      (if (< j (count (first matrix)))
        (let [new-matrix (process-element matrix i j)]
          (recur new-matrix i (inc j)))
        (recur matrix (inc i) 0))
      matrix))
)

(defn converged?
 "Еден од условите за конвергенција: ако нема промени во матрицата меѓу две итерации"
  [matrix]
  (= matrix (process-all-elements matrix))
)

(defn solve
  "Се изминуваат сите елементи на матрицата толку пати колку што е потребно т.е. додека сите елементи не се едночлени множества или доколку нема промени меѓу две итреации"
  [sudoku]
  (loop [matrix (transform sudoku)
         iterations 0
         max-iterations 1000]
    (if (and (<= iterations max-iterations) (or (not (converged? matrix))
                   (not (every? singleton-set? (flatten matrix)))))
      (recur (process-all-elements matrix) (inc iterations) max-iterations)
      matrix
    )
  )
)

(def example-sudoku
  [[1 0 2 3 0 4 0 0 0]
   [0 5 0 0 1 0 6 0 0]
   [0 7 0 0 0 5 0 0 8]
   [0 6 0 4 2 0 0 5 0]
   [3 0 9 0 0 0 2 0 7]
   [0 1 0 0 8 3 0 9 0]
   [4 0 0 6 0 0 0 3 0]
   [0 0 6 0 4 0 0 2 0]
   [0 0 0 9 0 1 7 0 4]]
  )


(defn set-to-int
  "Помошна функција за цртање"
  [s]
  (Integer. (apply str (sort s)))
)

(defn matrix-sets-to-ints 
  "Помошна функција за цртање"
  [matrix]
  (mapv (fn [row]
          (mapv set-to-int row))
        matrix)
)


(defn draw-matrix
  "Функција што го врши цртањето"
  [matrix]
  (q/background 255)
  (let [rows (count matrix)
        cols (count (first matrix))
        cell-width (/ 600 cols)
        cell-height (/ 600 rows)]
    (q/stroke 0)
    (doseq [i (range 1 rows)]
      (let [y (* i cell-height)]
        (q/stroke-weight (if (= (mod i 3) 0) 3 1))
        (q/line 0 y 600 y)))
    (doseq [j (range 1 cols)]
      (let [x (* j cell-width)]
        (q/stroke-weight (if (= (mod j 3) 0) 3 1))
        (q/line x 0 x 600)))
    (q/stroke-weight 1)
    (q/fill 0)
    (q/text-size 20)
       (doseq [i (range rows)
            j (range cols)]
      (let [value (get-in matrix [i j])
            x (* j cell-width)
            y (* i cell-height)]
        (q/stroke (if (zero? value) 0 200))
        (q/stroke-weight (if (zero? value) 1.5 2.5))
        (q/text-align :center :center)
        (q/text (str value) (+ x (/ cell-width 2)) (+ y (/ cell-height 2)))))))

(defn -main []
  (let [example-matrix
        (matrix-sets-to-ints (solve example-sudoku))]
    (q/defsketch example
      :draw (fn [] (draw-matrix example-matrix))
      :features [:keep-on-top]
      :size [600 600]))
)
