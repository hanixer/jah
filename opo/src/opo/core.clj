(ns opo.core)
(require '[clojure.string :as clstr])
(require 'clojure.java.io)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn gap
  []
  (println 'some))

(def negated-sum-str
  (comp str #(apply + %) #(map - %)))

(def camel-case->keyword
  (comp keyword
        (partial apply str)
        (partial interpose \-)
        #(map clstr/lower-case %)
        #(clstr/split % #"(?<=[a-z])(?=[A-Z])")))

(defn lazy-and-not
  [n]
  (let [s (range n)]
    (time (count s)))
  (let [l (apply list (range n))]
    (time (count l))))

(defn print-logger
  [writer]
  #(binding [*out* writer]
     (println %)))

(def *out*-printer (print-logger *out*))

(def str-writer (java.io.StringWriter.))
(def str-printer (print-logger str-writer))

(defn file-logger
  [file]
  #(with-open [f (clojure.java.io/writer file :append true)]
     ((print-logger f) %)))

(def file-printer (file-logger "sgary.log"))

(def orders
  [{:product "Clock", :customer "Wile Coyote", :qty 6, :total 300}
   {:product "Dynamite", :customer "Wile Coyote", :qty 20, :total 5000}
   {:product "Shotgun", :customer "Elmer Fudd", :qty 2, :total 800}
   {:product "Shells", :customer "Elmer Fudd", :qty 4, :total 100}
   {:product "Hole", :customer "Wile Coyote", :qty 1, :total 1000}
   {:product "Anvil", :customer "Elmer Fudd", :qty 2, :total 300}
   {:product "Anvil", :customer "Wile Coyote", :qty 6, :total 900}
   {:product "Anvil", :customer "Wile Coyote", :qty 2, :total 1288}])

(defn reduce-by
  [key-fn f init coll]
  (reduce (fn [summaries x]
            (let [k (key-fn x)]
              (assoc summaries k (f (summaries k init) x))))
          {} coll))

(defn reduce-by-in
  [key-fn f init coll]
  (reduce (fn [acc x]
            (let [ks (key-fn x)]
              (assoc-in acc ks
                        (f (get-in acc ks init) x))))
          {} coll))

(defn our-juxt
  [f g]
  (fn [x]
    [(f x) (g x)]))

(defn map-receiver-test
  [{:keys [a b]}]
  (list (list a) (list (list b))))

(reduce-by-in (juxt :customer :product)
           #(+ %1 (:total %2)) 0 orders)

(def flatten-data (reduce-by (juxt :customer :product)
                             #(+ %1 (:total %2)) 0 orders))

(reduce #(apply assoc-in %1 %2) {} flatten-data)

(defn naive-into1
  [to from]
  (reduce conj to from))

(naive-into1 [1 2 3] [2 3])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game of Conway

(defn empty-board
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  [board living-cells]
  (reduce (fn
            [board cells]
            (assoc-in board cells :on))
          board
          living-cells))

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn indexed-step
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board x 0 y 0]
      (cond
        (>= x w) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
         (let [new-cell
               (case (count-neighbours board [x y])
                 2 (get-in board [x y])
                 3 :on
                 nil)]
           (recur (assoc-in new-board [x y] new-cell) x (inc y)))))))

(defn indexed-step3
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce (fn
              [new-board [x y]]
              (let [new-cell
                    (case (count-neighbours board [x y])
                      2 (get-in board [x y])
                      3 :on
                      nil)]
                    (assoc-in new-board [x y] new-cell)))
              board
              (for [x (range w) y (range h)] [x y]))))


(defn window
  ([coll] (window nil coll))
  ([pad coll]
  (partition 3 1 (concat [pad] coll [pad]))))

(defn cell-block
  [[left mid right]]
  (window (map vector left mid right)))

(defn liveness
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block)))
             (if (= center :on) 1 0))
      2 center
      3 :on
      nil)))

(defn step-row
  [rows-triple]
  (vec (map liveness (cell-block rows-triple))))

(defn index-free-step
  [board]
  (vec (map step-row (window (repeat nil) board)))) 

(defn step
  [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
         :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))



(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maze generation
(defn new-maze-walls
  "Returns empty maze with all walls untouched"
  [w h]
  
  (reduce (fn [walls [x y]]
            ;(println (str "x = " x ", y = " y))
            (let [ds [-1 1]
                  with-x (for [d ds :when (< -1 (+ x d) w)]
                           
                           (do
                             ;(println (str "x " (+ x d)))
                             #{[x y] [(+ x d) y]}))
                  with-y (for [d ds :when (< -1 (+ y d) h)]
                           (do
                             ;(println (str "y " (+ y d)))
                             #{[x y] [x (+ y d)]}))]
              (into walls (concat with-x with-y))))
          #{} (for [x (range w) y (range h)]
                [x y])))

(defn grid
  [w h]
  (set (concat
        (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
        (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))

(defn new-maze
  [w h]
  {:size [w h] :walls (new-maze-walls w h)})

(defn print-maze
  [{[w h] :size walls :walls}]
  (doseq [r (range h)]
    (doseq [c (range w)]
      (print "x")
      (print (if (contains? walls #{[r c] [r (inc c)]}) "-" " ")))
    (println)
    (doseq [c (range w)]
      (print (if (walls #{[r c] [(inc r) c]}) "| " "  ")))
    (println)))

(defn maze
  [walls]
  (let [paths (reduce (fn [acc [a b] ]
                        (merge-with into acc {a [b] b [a]}))
                      {}
                      (map seq walls))
        initial (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) initial)]
      (let [rand-cell (rand-nth (seq unvisited))]
        (if rand-cell
          (let [walk (iterate (comp rand-nth paths) rand-cell)
                steps (zipmap (take-while unvisited walk) (next walk))]
            (recur (reduce disj walls (map set steps))
                   (reduce disj unvisited (keys steps))))
            walls)))))

(def test-maze (new-maze 3 3))

(defn draw-rand-maze
  [w h]
  (let [maze (maze (new-maze-walls w h))]
    `(draw w h maze)))

(defn draw
  [w h maze]
  (def scale-x 20)
  (def scale-y 20)
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
     (doto (proxy [javax.swing.JPanel] []
             (paintComponent [^java.awt.Graphics g]
               (let [g (doto ^java.awt.Graphics2D (.create g)
                         (.scale scale-x scale-y)
                         (.translate 1.5 1.5)
                         (.setStroke (java.awt.BasicStroke. 0.4)))]
                 (.drawRect g -1 -1 w h)
                 (doseq [[[xa ya] [xb yb]] (map sort maze)]
                   (let [[xc yc] (if (= xa xb)
                                   [(dec xa) ya]
                                   [xa (dec ya)])]
                     (.drawLine g xa ya xc yc))))))
       (.setPreferredSize (java.awt.Dimension.
                           (* scale-x (inc w)) (* scale-y (inc h))))))
    .pack
    (.setLocation (java.awt.Point. 500 300) )
    (.setVisible true)))

(defn draw-rand-maze
  [w h]
  (let [maze (maze (new-maze-walls w h))]
    (draw w h maze)))

;; Hex

(defn hex-vertices
  [w h]
  (set (for [y (range h) x (range (if (odd? y) 0 1) (* 2 w) 2)]
    [x y])))

(defn print-hex-vert
  [w h]
  (let [verts (set (hex-vertices w h))]
    (doseq [line (range h)]
      (doseq [col  (range w)]
        (print (if (verts [col line]) "O " "_ ")))
      (println))))

(defn hex-grid
  [w h]
  (let [vertices (hex-vertices w h)
        deltas [[2 0] [1 1] [-1 1]]]
    (set (for [v vertices d deltas f [+ -]
               :let [w (vertices (map f v d))]
               :when w] #{v w}))))

(defn hex-outer-walls
  [w h]
  (let [vertices (hex-vertices w h)
        deltas [[2 0] [1 1] [-1 1]]]
    (set (for [v vertices d deltas f [+ -]
               :let [w (map f v d)]
               :when (not (vertices w))] #{v (vec w)}))))

(defn maze-drawing-pane
  [w h maze]
  (def scale 10)
  (let [panel 
        (proxy [javax.swing.JPanel] []
          (paintComponent [^java.awt.Graphics g]
            (let [maze  (into maze (hex-outer-walls w h))
                  g (doto ^java.awt.Graphics2D (.create g)
                      (.scale scale scale)
                      (.translate 1.5 1.5)
                      (.setStroke (java.awt.BasicStroke. 
                                   0.4 java.awt.BasicStroke/CAP_ROUND
                                   java.awt.BasicStroke/JOIN_MITER)))
                  draw-line (fn [[[xa ya] [xb yb]]]
                              (.draw g
                                     (java.awt.geom.Line2D$Double. 
                                      xa (* 2 ya) xb (* 2 yb))))]
              (doseq [[[xa ya] [xb yb]] (map sort maze)]
                (draw-line 
                 (cond
                   (= ya yb) [[(inc xa) (+ ya 0.4)] [(inc xa) (- ya 0.4)]]
                   (< ya yb) [[(inc xa) (+ ya 0.4)] [xa (+ ya 0.6)]]
                   :else [[(inc xa) (- ya 0.4)] [xa (- ya 0.6)]]))))))]
    (.setPreferredSize panel (java.awt.Dimension. (* 2 scale (inc w)) (* 2 scale (inc h))))
    (.setVisible panel true)
    panel))

(defn hex-draw
  [w h maze]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane (maze-drawing-pane w h maze))
    .pack
    (.setVisible true)))
     
(defn maze-history
  [walls]
  (let [paths (reduce (fn [acc [a b] ]
                        (merge-with into acc {a [b] b [a]}))
                      {}
                      (map seq walls))
        initial (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) initial)
           hist [walls]]
      (let [rand-cell (rand-nth (seq unvisited))]
        (if rand-cell
          (let [walk (iterate (comp rand-nth paths) rand-cell)
                steps (zipmap (take-while unvisited walk) (next walk))]
            (recur (reduce disj walls (map set steps))
                   (reduce disj unvisited (keys steps))
                   (conj hist walls)))
          hist)))))

(defn somi
  [w h]
  (let [maze-hist (atom (maze-history (hex-grid w h)))
        frame (javax.swing.JFrame. "History")
        panel (javax.swing.JPanel.)
        btn (javax.swing.JButton. "Next")
        refresh (fn []
                  (.removeAll panel)
                  (.add panel btn)                 
                  (.add panel ^javax.swing.JPanel (maze-drawing-pane w h (first @maze-hist)))
                  (.revalidate panel)
                  (.repaint panel))]
    (.addActionListener 
     btn (proxy [java.awt.event.ActionListener] []
           (actionPerformed [_]
             (println "HHHHH")
             (swap! maze-hist rest)
             (if (empty? @maze-hist)
               (.setVisible btn false)
               (do
                 (swap! maze-hist rest)
                 (refresh))))))

    (refresh)
    (.setContentPane frame panel)
    (.pack frame)
    (.setVisible frame true)))

;(defn draw-wil
  ;[]
  ;(doto (javax.swing.JFrame. "Gertiop")
  ;  (doto (proxy [javax.swing.JPanel] []
            ;(paintComponent [^java.awt.Graphics g]
            ;  (let 

;; Path

(def labyrinth (let [g (grid 10 10)] (reduce disj g (maze g))))
(def theseus (rand-nth (distinct (apply concat labyrinth))))
(def minotaur (rand-nth (distinct (apply concat labyrinth))))

(require '[clojure.zip :as z])

(defn ariadne-zip
  [labyrinth loc]
  (let [paths (reduce (fn [acc [a b]] (merge-with into acc {a [b] b [a]})) {} (map seq labyrinth))
        children (fn [[from to]]
                   (seq (for [loc (paths to)
                              :when (not= loc from)]
                          [to loc])))]
    (z/zipper (constantly true)
              children
              nil
              [nil loc])))

(defn draw-with-path
  [w h maze path]
  (def scale-x 20)
  (def scale-y 20)
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
     (doto (proxy [javax.swing.JPanel] []
             (paintComponent [^java.awt.Graphics g]
               (let [g (doto ^java.awt.Graphics2D (.create g)
                         (.scale scale-x scale-y)
                         (.translate 1.5 1.5)
                         (.setStroke (java.awt.BasicStroke. 0.4)))]
                 (.drawRect g -1 -1 w h)
                 (doseq [[[xa ya] [xb yb]] (map sort maze)]
                   (let [[xc yc] (if (= xa xb)
                                   [(dec xa) ya]
                                   [xa (dec ya)])]
                     (.drawLine g xa ya xc yc)))
                 (.translate g -0.5 -0.5)
                 (.setColor g java.awt.Color/RED)
                 (doseq [[[xa ya] [xb yb]] path]
                   (.drawLine g xa ya xb yb)))))
       
       (.setPreferredSize (java.awt.Dimension.
                           (* scale-x (inc w)) (* scale-y (inc h))))))
    .pack
    (.setLocation (java.awt.Point. 500 300) )
    (.setVisible true)))

(defn test-path-draw
  []
  (let [w 40 h 40
        grid (grid w h)
        walls (maze grid)
        labyrinth (reduce disj grid walls)
        places (distinct (apply concat labyrinth))
        theseus (rand-nth places)
        mino (rand-nth places)
        path (->> theseus
                  (ariadne-zip labyrinth)
                  (iterate z/next)
                  (filter #(= minotaur (first (z/node %))))
                  first z/path rest)]
    (draw-with-path w h walls path)))

(defn test-zip
  []
  (def v [1 [2 [3 4]] [[[5 6 [7]]]] 8 [[[[[9 10]]]]]])
  (-> v z/vector-zip z/node println)
  (-> v z/vector-zip z/next z/node println)
  (-> v z/vector-zip z/down z/node println)
  (-> v z/vector-zip z/down z/next z/node println)
  (-> v z/vector-zip z/down z/next z/down z/node println)
  (-> v z/vector-zip z/down z/next z/next z/next z/node println))

(require 'clojure.xml)
(def xmlzipper (z/xml-zip (clojure.xml/parse "D:/home/f/SteelArmor.xml")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Concurency

(defn do-work-and-call-back
  [arg1 arg2 callback-fn]
  (future
    (Thread/sleep 5000)
    (callback-fn [(+ arg1 arg2) (- arg1 arg2)])))

(defn sync-fn
  [async-fn]
  (fn [& a]
    (let [result (promise)]
    (apply async-fn (conj (vec a) #(deliver result %&)))
    @result)))

(defn phone-numbers
  [string]
  (re-seq #"(\d{3})[.-]?(\d{3})[.-]?(\d{4})" string))

(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))

(defmacro wait-futures
  [& args]
  `(doseq [f# (futures ~@args)]
     @f#))

;; Watchers - we can add watcher functions(like Objservers) to the reference
;; This function will be called after change of reference value.
;; Watchers can be added and removed by keys using function add-watch, remove-watch

(defn echo-watcher
  [key identity old new]
  (println key  old "=>" new))

(defn log->list
  [destination key source old new]
  (when (not= old new)
    (swap! destination conj new)))

(defn test-watcher-history
  [] 
  (let [history (atom '())
        x (atom 1)]
    (add-watch x :hist (partial log->list history))
    (swap! x inc)
    (swap! x #(+ 3423423434234 %))
    (swap! x #(str "Hey" %))
    (println @history)))

;; Validators - obvious naming for reference types

(defn test-validators
  []
  (let [x (atom 3 :validator odd?)]
    (swap! x + 2)
    (println @x "OK)")
    `(swap! x - 1)))
        
;; "Game" :)




(defn loot
  [from to]
  (dosync
   (when-let [thing (first (:items @from))]
     (alter to update-in [:items] conj thing)
     (alter from update-in [:items] disj thing))))
    
(defn test-many-trans-alter
  []
  (let [x (ref 0)]
    (time
     (wait-futures 5
      (dotimes [_ 1000]
        (dosync (alter x + (apply + (range 1000)))))
      (dotimes [_ 1000]
        (dosync (alter x - (apply - (range 1000)))))))))

(defn test-many-trans-commute
  []
  (let [x (ref 0)]
    (time
     (wait-futures 5
                   (dotimes [_ 1000]
                     (dosync (commute x + (apply + (range 1000)))))
                   (dotimes [_ 1000]
                     (dosync (commute x - (apply - (range 1000)))))))))

(defn wrong-loot
  [from to]
  (dosync
   (when-let [thing (first (:items @from))]
     (commute to update-in [:items] conj thing)
     (commute from update-in [:items] disj thing))))

(defn attack
  [aggressor target]
  (dosync 
   (let [damage (* (rand 0.1) (:strength @aggressor))]
     (commute target update-in [:health] - damage))))

(defn heal
  [healer target]
  (dosync 
   (let [aid (* (rand 0.1) (:mana @healer))]
     (commute target update-in [:health] + aid))))

(def alive? (comp pos? :health))

(defn play
  [character action other]
  (while (and (alive? @character)
              (alive? @other)
              (action character other))
    (Thread/sleep (rand-int 50))))

(defn- enforce-max-health
  [{:keys [name health]}]
  (fn [character-data]
    (or (<= (:health character-data) health)
        (throw (IllegalStateException. (str name " is already at max health"))))))

(defn character
  [name & {:as opts}]
  (let [cdata (merge {:name name :items #{} :health 250}
                     opts)
        cdata (assoc cdata :max-health (:health cdata))
        validators (list* (enforce-max-health {:name name :health (:max-health cdata)})
                          (:validators cdata))]
    (ref (dissoc cdata :validators)
         ;:validator (fn [val] (every? (fn [pred] (pred val)) validators)))))
)))

(def gringo (character "Gringo" :health 567 :strength 200 :items (set (range 25))))
(def krokvel (character "Krokvel" :health 215 :strength 50))
(def alibe (character "Alibe" :health 25 :mana 100500 :items #{'filter}))

;; Vars - well... you can fill it later :)
;; ...

;; Atom - uncoordinated asynchronous modification, modifiers: send, send-off
(require '[clojure.java.io :as io])

(def console (agent *out*))
(def character-log (agent (io/writer "characters.log" :append true)))

(defn write-
  [^java.io.Writer w & content]
  (doseq [x (interpose " " content)]
    (.write w (str x)))
  (doto w
    (.write "\n")
    
    .flush))

(defn log-reference
  [reference & writer-agents]
  (add-watch reference :key
             (fn [_ reference old new]
               (doseq [writer-agent writer-agents]
                 (send-off writer-agent write- new)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro
(defmacro reversi
  [& forms]
  (clojure.walk/postwalk #(if (symbol? %)
                            (symbol (clojure.string/reverse (name %)))
                            %)
                         forms))

(defmacro single-wind
  [some]
  (list 'println some))

(defmacro my-while
  [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(defmacro hey
  [& body]
  "This macro is leaking abstraction...
   Confictls with let - let cannot accept fully qualified name"
  `(let [x :oop]
     ~@body))

(defmacro hey2
  [& body]
  "And now the same as above using gensym"
  (let [x-sym (gensym)]
    `(let [~x-sym :oop]
       ~@body)))

(defmacro hey2-shortened
  [& body]
  "And now the same as above using gensym underneath"
  `(let [x# :oop
         y# :uup]
     ~@body))

(defn prety
  [form]
  (clojure.pprint/pprint form)
  (println)
  (clojure.pprint/pprint (macroexpand-1 form)))

(defmacro our-doto
  [obj & forms]
  (let [obj-sym (gensym)]
  `(let [~obj-sym ~obj]
     ~@(map 
        (fn [f] 
          (if (list? f)
            (cons (first f) (cons obj-sym (rest f)))
            (list f obj-sym)))
        forms)
     ~obj-sym)))

(defmacro our-doto-second-try
  [obj & forms]
  (let [obj-sym (gensym)]
    `(let [~obj-sym ~obj]
       ~@(map (fn [f]
                (if (list? f)
                  (let [[f & args] f]
                    `(~f ~obj-sym ~@args))
                  `(~f ~obj-sym)))
              forms)
       ~obj-sym)))

(defmacro give-name-to-it
  [name it & body]
  (let [sym (gensym)]
    `(let [~sym ~it]
       (let [~name ~it]
          ~@body))))

(defmacro gogogo
  []
  `(println ~@(keys &env)))

(defmacro simplify
  [expr]
  (let [locals (set (keys &env))]
    (if (some locals (flatten expr))
      expr
      (do
        (println "Precompiting " expr)
        (list 'quote (eval expr))))))

(defmacro println-env
  [a]
  (println &form)
  (println [(type &env) (keys &env) &env])
  nil)

`(defn f1
  [a b]
  (println-env)
  [[a] b])

(defn fst-char-either
  [a b]
  (.substring ^java.lang.String (or a b) 0 1))

(defn macroexpand1-env [env form]
  (if-let [[x & xs] (and (seq? form) (seq form))]
    (if-let [v (and (symbol? x) (resolve x))]
      (if (-> v meta :macro)
        (apply @v form env xs)
        form)
      form)
    form))
  
(defmacro spy [expr]
  `(let [value# ~expr]
     (println (str "line #" ~(-> &form meta :line) ", " '~expr value#))
     value#))

(defmacro if-let-many [bindings then else]
  (reduce 
   (fn [acc [name value]]
     `(if-let [~name ~value]
        ~acc
        ~else))
   then
   (reverse (partition 2 bindings))))

(defn macroexpand1-env-v2 [env form]
  (if-let-many [[x & xs] (and (seq? form) (seq form))
               v (and (symbol? x) (resolve x))
               _ (-> v meta :macro)]
    (apply @v form env xs)
    form))
  
(defmacro thread [x & forms]
  (reduce 
   (fn [acc f]
     (if (list? f)
       `(~(first f) ~acc ~@(rest f))
       `(~f ~acc)))
   x
   forms))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols - corresponds to JAVA interfaces
(defprotocol Matrix
  (lookup [matrix i j])
  (update [matrix i j value])
  (rows [matrix])
  (cols [matrix])
  (dims [matrix]))

(extend-protocol Matrix
  clojure.lang.IPersistentVector
  (lookup [this i j]
    (get-in this [i j]))
  (update [this i j value]
    (update-in this [i j] value))
  (rows [this]
    (seq this))
  (cols [this]
    (apply map vector this))
  (dims [this]
    [(count this) (count (first this))]))

(defprotocol Measurable
  (width [this])
  (height [this]))

(defrecord Button [text])

(extend-type Button
  Measurable
  (width [btn]
    (* 8 (-> btn :text count)))
  (height [btn]
    8))

(def bordered
  {:width #(* 2 (:border-width %))
   :height #(* 2 (:border-height %))})

(defn combine [combinator f g]
  (fn [& args]
    (combinator
     (apply f args)
     (apply g args))))

(defrecord BorderedButton [text border-width border-height])

(extend BorderedButton
  Measurable
  (merge-with (partial combine +)
              (get-in Measurable [:impls Button])
              bordered))


`(defn get-subfolders [path]
  (let [f (java.io.File. path)]
    (->> (.listFiles f) seq (filter #(.isDirectory %)))))

`(defn good-dir? [dir]
  (prn (.getName dir))
  (some (fn [x]
           (and (not (.isDirectory x))
                (.contains (slurp x) wanted))) (file-seq dir)))

`(defn good-dirs []
  ;(filter #([%]) (get-subfolders scanned-path)))
  (filter good-dir? (get-subfolders scanned-path)))
