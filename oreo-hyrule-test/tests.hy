(import rich.traceback)
(.install rich.traceback)

(import oreo [eclair])
(import os [path :as osPath])

(setv funcs [])
(defn zoom [func] (.append funcs func))

(defn [zoom] 69611341-cf92-48bb-8cda-79c7fe28d9f2 []
              (import oreo [eclair])
              (for [i (eclair (range 100) "69611341-cf92-48bb-8cda-79c7fe28d9f2" "red")]))

(defn [zoom] 9a62edb0-a552-4283-914a-b4731968b1e4 []
              (import oreo [tea])
              (setv test (tea :a "b" :c "d"))
              (.append test "f")
              (.extend test "h" "j" "l")
              (.glue test "mnop")
              (.glue test (tea :q "r" :s "t"))
              (.glue test ["v" "x"])
              (assert (test) "b d f h j lmnopr tv x"))

(defn [zoom] 66c3744b-a036-4476-b1ae-024bc99bee41 []
              (import oreo [either?])
              (import collections [OrderedDict])
              (import addict [Dict])
              (assert (either? OrderedDict dict))
              (assert (either? Dict dict))
              (assert (either? OrderedDict Dict))
              (assert (= (either? "Dict" Dict) False))
              (assert (either? "Dict" str))
              (assert (= (either? "Dict" Dict list) False))
              (assert (either? "OrderedDict" "Dict"))
              (assert (either? "OrderedDict" "Dict" list)))

(defn [zoom] 554446e6-ffb6-4469-b4d2-90282e466751 []
              (import oreo [first-last-n])
              (setv ten (range 10))
              (assert (all (gfor i (range 5) (in i (first-last-n :iterable ten :number 5)))))
              (assert (all (gfor i (range 5 10) (in i (first-last-n :iterable ten :number 5 :last True))))))

(defn [zoom] a54f24d7-4620-4834-b996-04af2973926f []
              (import oreo [flatten])
              (setv nested #(1 #(2 #(3 #(4)))))
              (assert (= (flatten nested) [1 2 3 4]))
              (assert (= (flatten nested :times 0) nested))
              (assert (= (flatten nested :times 1) [1 2 #(3 #(4))]))
              (assert (= (flatten nested :times 2) [1 2 3 #(4)]))
              (assert (= (flatten nested :times 3) [1 2 3 4])))

(for [func (eclair funcs "tests" "blue")] (func))
