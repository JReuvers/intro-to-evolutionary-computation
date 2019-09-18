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
  [b]
  (+ (Math/abs(- 0 (get b 0))) (Math/abs(- 0 (get b 1)))))

(defn search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children]}
   start-node max-calls]
  (loop [frontier [start-node]
         came-from {start-node :start-node}
         num-calls 0]
    ; (println num-calls ": " frontier)
    ; (println came-from)
    (println "Frontier: " frontier)
    ;(println "Start Node: " start-node)
    (let [current-node (get-next-node frontier)]
    ;(println "Current Node: " current-node)
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
                    ;(println "Kids: " kids)
          (let [priority-frontier (reverse (vec
                  (into (pq/priority-queue #(manhattan-distance-heuristic %)) kids)))]
          (recur priority-frontier
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls))))))))
