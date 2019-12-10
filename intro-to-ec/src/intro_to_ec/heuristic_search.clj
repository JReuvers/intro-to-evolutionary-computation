(ns intro-to-ec.heuristic-search
  (:require [clojure.set :as cset]
    [shams.priority-queue :as pq]))

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(def depth-first-search
  {:get-next-node first
   :add-children concat})

(def breadth-first-search
  {:get-next-node first
   :add-children #(concat %2 %1)})

(def random-search
  {:get-next-node rand-nth
   :add-children concat})

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(defn manhattan-distance-heuristic
  [current-node]
  (+ (Math/abs(- 0 (get current-node 0))) (Math/abs(- 0 (get current-node 1)))))

(defn euclidean-distance-heuristic
  [current-node]
  (Math/sqrt (+ (* 2 (Math/abs (- 0 (get current-node 0)))) (* 2 (Math/abs (- 0 (get current-node 1)))))))

(defn chebyshev-distance-heuristic
  [current-node]
  (max (Math/abs (- 0 (get current-node 0))) (Math/abs (- 0 (get current-node 1)))))

(defn a-star-heuristic-manhattan
  [current-node cost-so-far]
  (+ (manhattan-distance-heuristic current-node) cost-so-far))

(defn a-star-heuristic-euclidean
  [current-node cost-so-far]
  (+ (euclidean-distance-heuristic current-node) cost-so-far))

(defn a-star-heuristic-chebyshev
  [current-node cost-so-far]
  (+ (chebyshev-distance-heuristic current-node) cost-so-far))

(defn heuristic-search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children]}
   start-node
   max-calls]
  (loop [frontier [start-node]
         came-from {start-node :start-node}
         num-calls 0]
    (println "Frontier: " frontier " Num Calls: " num-calls)
    (let [current-node (get-next-node frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
                    (println "Kids: " kids " Num Calls: " num-calls)
          (let [priority-frontier (reverse (vec
                  (into (pq/priority-queue #(manhattan-distance-heuristic %)) kids)))]
            (recur priority-frontier
             (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
             (inc num-calls))))))))

; ***************
; To run: 
; (require '[intro-to-ec.heuristic-search :as hs])
; (require '[intro-to-ec.grid-problem-with-walls :as walls])
; (hs/a-star-search hs/breadth-first-search (walls/make-grid-problem -10 10 #{}) hs/a-star-heuristic-<NAME OF FORMULA> [3 3] 100)
; ***************
(defn a-star-search
 [{:keys [get-next-node add-children]}
  {:keys [goal? make-children]}
  heuristic-function
  start-node
  max-calls]
 (loop [frontier [start-node]
        came-from {start-node :start-node}
        cost-so-far {start-node 0}
        num-calls 0]
   (println "Frontier: " frontier " Num Calls: " num-calls)
   (let [current-node (get-next-node frontier)]
     (cond
       (goal? current-node) (generate-path came-from current-node)
       (= num-calls max-calls) :max-calls-reached
       :else
       (let [kids (remove-previous-states
                   (make-children current-node) frontier (keys came-from))]
                   (println "Kids: " kids " Num Calls: " num-calls)
         (let [updated-cost (+ 1 (get cost-so-far current-node))]
           (let [priority-frontier (reverse (vec
                   (into (pq/priority-queue #(heuristic-function % updated-cost)) (add-children kids (rest frontier)))))]
             (recur priority-frontier
              (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
              (reduce (fn [cost child] (assoc cost child updated-cost)) cost-so-far kids)
              (inc num-calls)))))))))
