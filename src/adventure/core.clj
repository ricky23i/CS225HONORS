(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:Harambe-Memorial {:desc "The grave of the Great Harambe. All praise Harambe! "
           :title "Great Harambe Memorial"
           :dir {:south :Such-Path}
           :contents #[]}
   :Such-Path {:desc "The main path of Doge Island."
              :title "Such Path"
              :dir {:south :Harambe-Memorial
					:north :Doge-Village
					:west :Wow-Forest}
               :contents #[Doge-Treat]}
   :Wow-Forest {:desc "The local forest of the Doge. Rumors say a holy Doge Sword is deep within the forest"
                :title "Wow Forst"
                :dir {:east:Such-Path}
                :contents #[]}
   :Doge-Village {:desc "The Doge Village where all doge live and prosper"
                :title "Doge Village"
                :dir {:south:Such-Path}
                :contents #[]}
   :Much-Ruins {:desc "Ruins of an Ancient city. There stands a wall with a missing jew in the middle."
                :title "Much Ruins"
                :dir {:west:Such-Path}
                :contents #[Doge-Coin]}
   :So-Lost {:desc "Such forest, very lost, Wow"
                :title "Wow Forest:So Lost"
                :dir {:north:Wow-Forest}
                :contents #[Doge-Toy]}
   :Very-Close {:desc "Ruins of an Ancient city. There stands a wall with a missing jew in the middle."
                :title "Wow Forest:Very Close"
                :dir {:east:Wow-Forest}
                :contents #[]}
   })

(def adventurer
  {:location : Harambe-Memorial
   :inventory #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn tock [player]
  (update-in player [:tick] inc))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)

         _ (do (println "I don't understand you.")
               player)

         )) 

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
