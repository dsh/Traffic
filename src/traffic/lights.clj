(ns traffic.lights)

; Original code had no facilities for changing lights. I added a new agent to cover that.
; Alternative solution should be to keep some state in steps to determine how many steps
; have executed and thus time to switch lights.

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
  ; If we pass in intersection would could also allow left turns if there is no opposing traffic.
  ; We dont' right now, and this is causing backlogs as left turns block the queue.
  ; We could also create a "left turn" lane. We'd need to change street from Queue to something else.
  [lights from direction]
  (let [light (get lights from)
        ]
    (case light
      :red false
      :green (case direction
               ; no car
               nil false

               ; Only allow left turns if all other roads have reds.
               :left (every? (partial = :red) (vals (dissoc lights from)))

               ; always allow straight and right turns
               true
               )
      )))

(defn make-lights
  "lights is a map of directions and light color."
  []
  (agent
    {:north :green
     :south :red
     :east :red
     :west :red}
    ))

(defn change-lights [lights]
  (let [north (= :green (:north lights))
        south (= :green (:south lights))
        east  (= :green (:east  lights))
        west  (= :green (:west  lights))
        new-lights          (cond
                              ; north green -> north & south green
                              (and north (not south)) (assoc lights :south :green)
                              ; north and south green -> south green
                              (and north south)       (assoc lights :north :red)
                              ; south green -> east green
                              (and (not north) south) (assoc lights :south :red :east :green)
                              ; east green -> east & west green
                              (and east (not west))   (assoc lights :west :green)
                              ; east and south green -> south green
                              (and east west)         (assoc lights :east :red)
                              ; west green -> north green
                              (and (not east) west)   (assoc lights :west :red :north :green))]
    (do (println new-lights) new-lights)))

; from http://stackoverflow.com/a/21404281/895588
(defn periodically
  [f interval]
  (Thread.
    #(try
      (while (not (.isInterrupted (Thread/currentThread)))
        (Thread/sleep interval)
        (f))
      (catch InterruptedException _))))


(defn light-controller
  "Create a thread that changes the lights."
  [lights]
  (periodically #(send lights change-lights) (* 10 interval)))


(defn four-way-intersection []
  {:streets {:north (doto (make-street)
                      (populate-street 10))
             :south (doto (make-street)
                      (populate-street 10))
             :east (doto (make-street)
                     (populate-street 10))
             :west (doto (make-street)
                     (populate-street 10))}
   :lights (make-lights)})


(defn traffic-generator
  "Create a thread that adds new cars to the streets."
  [streets]
  (periodically #(doseq [s streets] (populate-street s 1)) interval))





(defn move-car [from direction]
  (println "Car from" from "went" direction))

(defn step [intersection]
  ;; @todo do this in a dosync? I think I'm safe since I deref lights at last second, but I'm not positive.
  (let [lights (get intersection :lights)]
    (doseq [keyval (get intersection :streets)]
      (let [from (key keyval)
            street (val keyval)
            direction (first @street)]
        (when (allowed? @lights from direction)
          (do
            (move-car from direction)
            (send street pop)))))))

(defn -main [steps]
  (let [steps (Integer. steps)
        intersection (four-way-intersection)
        generator (traffic-generator (vals (:streets intersection)))
        controller (light-controller (:lights intersection))
        ]
    (println "Starting...")
    (.start generator)
    (.start controller)
    (dotimes [n steps]
      (step intersection)
      (Thread/sleep interval))
    (doseq [keyval (:streets intersection)] (println (count @(val keyval)) "remaining for" (key keyval)))
    (println "Done. Cleaning up...")
    (.stop generator)
    (.stop controller)))
