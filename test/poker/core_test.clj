(ns poker.core-test
  (:require [clojure.test :refer :all]
            [poker.core :refer :all]))

(deftest test-suit
  (is (= 
        \D
        (suit "2D")
  ))

  (is (= 
        \H
        (suit "AH")
  ))

  (is (= 
        \C
        (suit "KC")
  ))

  (is (= 
        \S
        (suit "QS")
  ))

  (is (= 
        \S
        (suit "TS")
  ))
)

(deftest test-rank
  (is (= 
        7
        (rank "7D")
  ))

  (is (= 
        14
        (rank "AS")
  ))

  (is (= 
        12
        (rank "QC")
  ))

  (is (= 
        10
        (rank "TS")
  ))

  (is (= 
        11
        (rank "JS")
  ))
)

(deftest test-pair?
  (is (= 
        true
        (pair? ["2H" "3S" "4C" "4D" "7D"]) ; 2ki
  ))

  (is (= 
        true
        (pair? ["10S" "QS" "KH" "2C" "KD"]) ; par K
  ))

  (is (= 
        false
        (pair? ["2H" "3S" "4C" "4D" "4H"]) ; 3 isti
  ))

  (is (= 
        false
        (pair? ["2H" "3S" "4C" "5C" "7D"]) ; nisto
  ))

  (is (= 
        true
        (pair? ["JH" "JS" "4C" "5C" "7D"]) ; par dzandari
  ))
)

(deftest test-three-of-a-kind?
  (is (= 
        true
        (three-of-a-kind? ["2H" "3S" "4C" "4D" "4S"]) ; 4ki
  ))

  (is (= 
        false
        (three-of-a-kind? ["10S" "QS" "KH" "2C" "KD"]) ; par K
  ))

  (is (= 
        true
        (three-of-a-kind? ["2H" "2S" "4C" "5C" "2D"]) ; 3ki
  ))

  (is (= 
        false
        (three-of-a-kind? ["2H" "3S" "4C" "5C" "7D"]) ; nisto
  ))

  (is (= 
        true
        (three-of-a-kind? ["5H" "5S" "5C" "7D" "9D"]) ; 5ki
  ))
)

(deftest test-four-of-a-kind?
  (is (= 
        true
        (four-of-a-kind? ["2H" "2S" "2C" "2D" "7D"]) ; 2ki
  ))

  (is (= 
        false
        (four-of-a-kind? ["10S" "QS" "KH" "2C" "KD"]) ; par K
  ))

  (is (= 
        true
        (four-of-a-kind? ["2H" "2S" "2C" "2D" "KD"]) ; 2ki
  ))

  (is (= 
        false
        (four-of-a-kind? ["2H" "3S" "4C" "5C" "7D"]) ; nisto
  ))

  (is (= 
        true
        (four-of-a-kind? ["5H" "5S" "5C" "5D" "9D"]) ; 5ki
  ))
)

(deftest test-straight?
  (is (= 
        true
        (straight? ["2H" "3S" "4C" "5C" "6D"]) ; [2,6]
  ))

  (is (= 
        true
        (straight? ["TS" "QS" "KH" "JD" "9D"]) ; [9,K]
  ))

  (is (= 
        false
        (straight? ["2H" "3S" "4C" "5C" "7D"]) ; nisto
  ))

  (is (= 
        true
        (straight? ["AH" "2S" "3C" "4D" "5D"]) ; [A,5]
  ))

  (is (= 
        true
        (straight? ["TH" "JS" "QC" "KD" "AD"]) ; [T,A]
  ))
)

(deftest test-flush?
  (is (= 
        true
        (flush? ["2H" "4H" "5H" "9H" "7H"]) ; 5 srca
  ))

  (is (= 
        false
        (flush? ["TS" "QS" "KH" "JD" "9D"]) ; skala bez boja
  ))

  (is (= 
        false
        (flush? ["2H" "3S" "4C" "5C" "7D"]) ; skala bez boja
  ))

  (is (= 
        false
        (flush? ["AH" "2H" "3H" "4H" "5H"]) ; skala vo boja
  ))

  (is (= 
        false
        (flush? ["TH" "JS" "QC" "KD" "AD"]) ; skala bez boja
  ))
)

(deftest test-straight-flush?
  (is (= 
        true
        (straight-flush? ["2H" "3H" "4H" "5H" "6H"]) ; [2,6] srca
  ))

  (is (= 
        false
        (straight-flush? ["10S" "QS" "KH" "JD" "9D"]) ; skala bez boja
  ))

  (is (= 
        false
        (straight-flush? ["2H" "3S" "4C" "5C" "7D"]) ; nisto
  ))

  (is (= 
        true
        (straight-flush? ["AH" "2H" "3H" "4H" "5H"]) ; [A,5] srca
  ))

  (is (= 
        false
        (straight-flush? ["10H" "JS" "QC" "KD" "AD"]) ; skala bez boja
  ))
)

(deftest test-full-house?
  (is (= 
        true
        (full-house? ["2H" "2S" "4C" "4D" "4S"]) ; 2ki so 4ki
  ))

  (is (= 
        false
        (full-house? ["10S" "QS" "KH" "JD" "9D"]) ; skala
  ))

  (is (= 
        false
        (full-house? ["2H" "3S" "4C" "5C" "7D"]) ; nisto
  ))

  (is (= 
        false
        (full-house? ["AH" "2H" "3H" "4H" "4S"]) ; skala vo boja
  ))

  (is (= 
        true
        (full-house? ["10H" "10S" "QC" "QD" "QH"]) ; 10ki so dami
  ))
)

(deftest test-two-pairs?
  (is (= 
        true
        (two-pairs? ["2H" "2S" "4C" "4D" "7D"]) ; 2ki so 4ki
  ))

  (is (= 
        false
        (two-pairs? ["TS" "QS" "KH" "JD" "9D"]) ; skala
  ))

  (is (= 
        false
        (two-pairs? ["2H" "3S" "4C" "5C" "8H"]) ; nisto
  ))

  (is (= 
        false
        (two-pairs? ["7H" "5H" "3H" "4S" "4D"]) ; par 4ki
  ))

  (is (= 
        false
        (two-pairs? ["TH" "JS" "QC" "AH" "AD"]) ; par asovi
  ))
)


(deftest test-value
  (is (= 
        8
        (value ["2H" "3H" "4H" "5H" "6H"]) ; skala vo boja
  ))

  (is (= 
        7
        (value ["2H" "2S" "2C" "2D" "7D"]) ; 4 2ki
  ))

  (is (= 
        6
        (value ["2H" "2S" "4C" "4D" "4S"]) ; full house 2ki so 4ki
  ))

  (is (= 
        5
        (value ["2H" "4H" "5H" "9H" "7H"]) ; boja srca
  ))

  (is (= 
        4
        (value ["2H" "3S" "4C" "5C" "6D"]) ; skala bez boja
  ))
)

(deftest test-kickers
  (is (= 
        '(5 9)
        (kickers ["5H" "5S" "5C" "5D" "9D"]) ; za cetiri 5ki
  ))

  (is (= 
        '(14 13 12 11 10)
        (kickers ["AS" "KS" "QS" "JD" "TD"]) ; za skala bez boja
  ))

  (is (= 
        '(4 2 7)
        (kickers ["2H" "2S" "4C" "4D" "7D"]) ; za dva para 2ki so 4ki
  ))

  (is (= 
        '(14 11 10)
        (kickers ["AH" "AS" "AD" "JD" "TD"]) ; za tri asovi
  ))

  (is (= 
        '(10 13)
        (kickers ["TH" "TS" "TC" "KD" "KH"]) ; za full house 10ki so kralovi
  ))
)

(deftest test-higher-kicker?
  (is (= 
        true
        (higher-kicker? '(5 4 3 2) '(4 3 2 1)) ; prva pogolema
  ))

  (is (= 
        nil
        (higher-kicker? '(14 13 12 11) '(14 13 12 11)) ; isti
  ))

  (is (= 
        true
        (higher-kicker? '(10 9) '(8 7)) ; prva pogolema
  ))

  (is (= 
        false
        (higher-kicker? '(12 11) '(12 13)) ; prva pomala zaradi vtoriot el
  ))

  (is (= 
        nil
        (higher-kicker? '() '()) ; prazni listi 
  ))
)

(deftest test-beats?
  (is (= 
        false
        (beats? ["2H" "3S" "4C" "5C" "6D"] ["TS" "QS" "KH" "JD" "9D"])
  )) ; vtorata skala e pojaka

  (is (= 
        true
        (beats? ["TS" "TH" "KH" "KD" "KS"] ["2H" "3S" "4C" "5C" "6D"])
  )) ; full house > skala

  (is (= 
        false
        (beats? ["AH" "2H" "3H" "4H" "4S"] ["TH" "JS" "QC" "KD" "AD"])
  )) ; skala > par 4ki

  (is (= 
        true
        (beats? ["TH" "TS" "TC" "TD" "AD"] ["AH" "AD" "AS" "4H" "4S"])
  )) ; 4 10ki > full house asovi so 4ki

  (is (= 
        true
        (beats? ["2H" "2S" "4C" "4D" "4H"] ["AH" "2H" "3H" "4H" "7H"])
  )) ; full house 2ki so 4ki > boja srca
)

(deftest test-winning-hand
  (is (= ; boja > skala > nisto
        flush-hand
        (winning-hand high-seven straight-hand flush-hand)
  ))

  (is (= ; straight flush so podobar kicker
        high-ace-straight-flush-hand
        (winning-hand low-ace-straight-flush-hand high-ace-straight-flush-hand)
  ))

  (is (= ; 4 isti > full house > 3 isti
        four-of-a-kind-hand
        (winning-hand four-of-a-kind-hand three-of-a-kind-hand full-house-hand)
  ))

  (is (= ; skalata so podobar kicker > skalata so polos > 2 para
        high-ace-straight-hand
        (winning-hand high-ace-straight-hand low-ace-straight-hand two-pairs-hand)
  ))

  (is (= ; bez argumenti vrakja nil
        nil
        (winning-hand)
  ))
)
