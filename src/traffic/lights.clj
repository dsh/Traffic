(ns traffic.lights)

(def ^{:doc "Time to sleep between steps in milliseconds."} interval 100)

(defn make-street
  "A street is a queue containing zero or more cars."
  []
  (agent clojure.lang.PersistentQueue/EMPTY))

(defn populate-street
  "Load up a street. Each car is a keyword indicating the direction it's going."
  [street car-count]
  (dotimes [_ car-count]
    (send street conj (rand-nth [:straight :straight :left :right]))))

(def directions {:north {:straight :south :right :west :left :east}
                 :south {:straight :north :right :east :left :west}
                 :east {:straight :west :right :north :left :south}
                 :west {:straight :east :right :south :left :north}})

(defn allowed?
  [lights from direction]
  ;; TODO: write
  )

(defn four-way-intersection []
  {:streets {:north (doto (make-street)
                      (populate-street 10))
             :south (doto (make-street)
                      (populate-street 10))
             :east (doto (make-street)
                     (populate-street 10))
             :west (doto (make-street)
                     (populate-street 10))}
   :lights (atom {:north :green
                  :south :red
                  :east :red
                  :west :red})})

(defn traffic-generator
  "Create a thread that adds new cars to the streets."
  [streets]
  (Thread. #(doseq [s streets]
              (populate-street s 1)
              (Thread/sleep interval))))

(defn move-car [from direction]
  (println "Car from" from "went" direction))

(defn step [intersection]
  ;; TODO: write
  )

(defn -main [steps]
  (let [steps (Integer. steps)
        intersection (four-way-intersection)
        generator (traffic-generator (vals (:streets intersection)))]
    (.start generator)
    (dotimes [n steps]
      (step intersection)
      (Thread/sleep interval))))
