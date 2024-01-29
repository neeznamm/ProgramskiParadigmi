# ПП домашни 5 и 6, зад. 1 - Судоку

## Користење
    
    $ cd target/uberjar
    $ java -jar domasna56-0.1.0-standalone.jar 
    
или со двоен клик на jar фајлот.

## Компајлирање

За компајлирање е потребен [Leiningen](https://leiningen.org/) што се справува со dependency-то за библиотеката за цртање. Потребное само во работниот директориум да се изврши `lein deps`. Главната класа (`-main`) ја повикува `solve` функцијата врз влезната матрица (`example-sudoku`) и го исцртува резултатот. `-main` ја повикуваме со `lein run`. Тестовите се извршуваат со `lein test`. Се пакува во uberjar што ги содржи сите зависности со `lein uberjar`.

## Пример
```clojure
(= 
[[#{1} #{8} #{2} #{3} #{6} #{4} #{5} #{7} #{9}]
 [#{9} #{5} #{3} #{8} #{1} #{7} #{6} #{4} #{2}]
 [#{6} #{7} #{4} #{2} #{9} #{5} #{3} #{1} #{8}]
 [#{8} #{6} #{7} #{4} #{2} #{9} #{1} #{5} #{3}]
 [#{3} #{4} #{9} #{1} #{5} #{6} #{2} #{8} #{7}]
 [#{2} #{1} #{5} #{7} #{8} #{3} #{4} #{9} #{6}]
 [#{4} #{9} #{1} #{6} #{7} #{2} #{8} #{3} #{5}]
 [#{7} #{3} #{6} #{5} #{4} #{8} #{9} #{2} #{1}]
 [#{5} #{2} #{8} #{9} #{3} #{1} #{7} #{6} #{4}]]
 (solve 
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
)
-> true
```

Алгоритмот од барањата не гарнатира решение (сите множества да ги трансформира во едночлени) и за судокуа кои имаат еднолично решение (било кое од [sudoku.net](www.sudoku.net).
