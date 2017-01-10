(ns Player
  (:gen-class)
  (require clojure.set))

(defn next-nodes [curr links seen]
  (let [others 
        (map (fn [link] 
               (if (link curr)
                 (first (clojure.set/difference link #{curr}))
                 nil)) links)]
    (filter 
     (fn [other] 
       (and other
            (not (seen other)))) others)))

(defn backtrack [curr start parents]
  (loop [node curr
         path []]
    (if (= node start)
      (reverse (conj path start))
      (if (not (contains? parents node))
        (throw (Exception. "parents does not contains node"))
        (recur (parents node) (conj path node))))))

(defn bfs [start target links]
  (letfn [(impl [curr queue seen parents]
            (if (= curr target)
              (backtrack curr start parents)
              (let [next-nds (next-nodes curr links seen)
                    queue-new (into queue next-nds)
                    seen-new (into seen next-nds)]
                (if (empty? queue-new)
                  nil
                  (impl 
                   (first queue-new) 
                   (vec (next queue-new))
                   seen-new
                   (reduce (fn [prnts node] (assoc prnts node curr))
                           parents next-nds))))))]
    (impl start [] #{start} {})))

(defn read-links [L]
  (loop [i L
         links #{}]
    (if (> i 0)
      (let [N1 (read) N2 (read)]
        
        (recur (dec i)
               (conj links (into #{} [N1 N2]))))
      links)))

(defn read-gateways [E]
  (loop [i E
         gws #{}]
    (if (> i 0)
      (recur (dec i) 
             (conj gws (read)))
      gws)))

(defn print-link [link]
  (if (= (count link) 1)
    (println (first link) (first link))
    (println (first link) (second link))))

(defn find-shortest [paths]
  (when (empty? paths)
    (throw (Exception. "paths cannot be empty")))
  (reduce 
   (fn [shortest curr] 
     (if (and (> (count shortest) (count curr)) 
              (not (nil? curr))
              (not (empty? curr)))
       curr 
       shortest)) 
   (first paths)
   paths))

(defn pick-link [shortest]
  (let [shortest (reverse shortest)]
  (if (= (count shortest) 1)
    #{(first shortest)}
    #{(first shortest) (second shortest)})))

(defn solve [SI links gateways]
    (let [paths (filter #(not (nil? %)) (map (fn [g] (bfs SI g links)) gateways))
          shortest (find-shortest paths)          
          link (pick-link shortest)]
      link))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main [& args]
  (let [N (read) L (read) E (read)]
    ; N: the total number of nodes in the level, including the gateways
    ; L: the number of links
    ; E: the number of exit gateways
    (let [links (read-links L)
          gs (read-gateways E)]
      (loop [SI (read)
             links links]
        (let [link (solve SI links gs)]
          (print-link link)
          (recur (read)
                 (disj links link)))))))
