(ns poker.core
  (:gen-class))

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn suit
 "Вториот карактер од стрингот"
 [card]
 (nth card 1)
)

(defn rank
  "Ако првиот карактер од стрингот е цифра, ја кастираме во int, во спротивно ја бараме вредноста од rank-map со клуч rank"
  [card]
  (let [rank (nth card 0)]
  (if (Character/isDigit rank) (Character/digit rank 10) (rank-map rank))
  )
)

(defn pair?
  "Првиот услов во and клаузулата бара да има точно еден пар со иста вредност (пр. за да одбиеме 2+2), а вториот бара покрај парот да нема спарено поголем број карти (пр. за да одбиеме 2+3)"
  [hand]
  (and (= (count (filter #(= 2 %) (vals (frequencies (map rank hand))))) 1) 
       (= (apply max (vals (frequencies (map rank hand)))) 2)) 
)

(defn three-of-a-kind?
  "Првиот услов во and клаузулата бара да нема парови, а вториот бара да има три карти со иста вредност и тоа да е најголемиот број карти со иста вредност"
  [hand]
   (and (= (count (filter #(= 2 %) (vals (frequencies (map rank hand))))) 0) 
       (= (apply max (vals (frequencies (map rank hand)))) 3)) 
)

(defn four-of-a-kind?
  "Бараме да има 4 карти со иста вредност и тоа да е најголемиот број спарени карти."
  [hand]
    (= (apply max (vals (frequencies (map rank hand)))) 4) 
)

(defn straight?
  "Бараме сортираната низа вредности на картите да формира опсег [najmala_vrednost, najmala_vrednost+4]. Се опфаќа и посебниот случај кога A треба да се рачуна како 1, а тоа е само ако сортираната низа е '(2 3 4 5 14). Не може да бараме да не е flush? зашто flush бара да не е straight? (deadlock)."
  [hand]
  (let [sorted-ranks (sort (map rank hand))]
   (or (= (range (first sorted-ranks) (+ (first sorted-ranks) 5)) sorted-ranks)
       (= '(2 3 4 5 14) sorted-ranks)
   )
  )
)

(defn flush?
  "Бараме (vals (frequencies (map (suit hand)))) да е '(5), што значи мапата мапата {suit1: count1, suit2: count2 ...} да има само еден пар клуч-вредност, а тоа да е {S: 5} за било која боја S. Притоа бараме картите да не формираат скала."
  [hand]
  (and (not (straight? hand)) (= (vals (frequencies (map suit hand))) '(5)))
)

(defn straight-flush?
  "Аналогно на flush? со тоа што сега бараме картите да формираат скала."
  [hand]
  (and (straight? hand) (= (vals (frequencies (map suit hand))) '(5)))
)

(defn full-house?
  "Бараме да има еден пар карти со иста вредност и една тројка карти со иста вредност."
  [hand]
   (and (= (count (filter #(= 2 %) (vals (frequencies (map rank hand))))) 1) 
       (= (apply max (vals (frequencies (map rank hand)))) 3)) 
)

(defn two-pairs?
  "Бараме да има два пара карти со иста вредност (нема потреба од додатни барања)."
  [hand]
   (= (count (filter #(= 2 %) (vals (frequencies (map rank hand))))) 2) 
)

(defn value
  "Пребаруваме во опаѓачки редослед за да не даде 4 (straight) ако е straight-flush."
  [hand]
  (cond
  (straight-flush? hand) 8
  (four-of-a-kind? hand) 7
  (full-house? hand) 6
  (flush? hand) 5
  (straight? hand) 4
  (three-of-a-kind? hand) 3
  (two-pairs? hand) 2
  (pair? hand) 1
  :else 0
  )
)

(defn kickers
  "Како што е наведено во hint-от: ги земаме вредностите на картите со фреквенција 4 (сортирани опаѓачки по самата вредност), на крајот на оваа листа ги лепиме вредностите со фреквенција 3 (исто сортирани) итн. Сигурно има поелегантно решение ама ова најбрзо се куца :("
  [hand]
  (concat  (reverse (sort (map first (filter #(= 4 (second %)) (frequencies (map rank hand))))))
           (reverse (sort (map first (filter #(= 3 (second %)) (frequencies (map rank hand))))))
           (reverse (sort (map first (filter #(= 2 (second %)) (frequencies (map rank hand)))))) 
           (reverse (sort (map first (filter #(= 1 (second %)) (frequencies (map rank hand)))))))
)

(defn higher-kicker?
  "Рекурзивно ги изминуваме двете лист со rest додека не стигнеме до првата нееднаквост."
  [kicker1 kicker2]
  (cond
  (or (empty? kicker1) (empty? kicker2)) nil
  (> (first kicker1) (first kicker2)) true
  (< (first kicker1) (first kicker2)) false
  :else (higher-kicker? (rest kicker1) (rest kicker2))
  )
)

(defn beats?
  "Ако value им се исти се споредуваат според kickers"
  [hand1 hand2]
  (cond
  (> (value hand1) (value hand2)) true
  (< (value hand1) (value hand2)) false
  :else (higher-kicker? (kickers hand1) (kickers hand2))
  )
)

(defn beats-reducer
  [hand1 hand2]
  (if (beats? hand1 hand2) hand1 hand2)
)

(defn winning-hand
  "Reduce врз листата аргументи со функцијата beats-reducer која ја враќа победничката комбинација. Слично на reducer функцијата за најдолг стринг од зад. 2в, домашна 4."
  [& hands]
  (if (= 0 (count hands)) nil (reduce beats-reducer hands))
) 

(def high-seven ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])
