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
                :dir {:east :Such-Path}
                :contents #[]}
   :Doge-Village {:desc "The Doge Village where all doge live and prosper"
                :title "Doge Village"
                :dir {:south :Such-Path}
                :contents #[]}
   :Much-Ruins {:desc "Ruins of an Ancient city. There stands a wall with a missing jew in the middle."
                :title "Much Ruins"
                :dir {:west :Such-Path}
                :contents #[Doge-Coin]}
   :So-Lost {:desc "Such forest, very lost, Wow"
                :title "Wow Forest:So Lost"
                :dir {:north :Wow-Forest}
                :contents #[Doge-Toy]}
   :Very-Close {:desc "Ruins of an Ancient city. There stands a wall with a missing jew in the middle."
                :title "Wow Forest:Very Close"
                :dir {:east :Wow-Forest
					  :south :Not-Right}
                :contents #[]}
   :Not-Right {:desc "Deep within the Wow Forest. You can see a bright light towards the right side of the forest"
                :title "Wow Forest: Definitely Not Right"
                :dir {:north :Very-Close
					  :south :Wow}
                :contents #[]}
   :Wow {:desc "Doge Temple is seen from here. Just north of Wow Forest: Wow"
                :title "Wow Forest: Wow"
                :dir {:north :Not-Right
					  :south :Doge-Temple}
                :contents #[]}
   :Nope-Path {:desc "Nope definitely not here"
                :title "Nope Path"
                :dir {:east :Not-Right}
                :contents #[Mysterious-Crystal]}
   :Doge-Temple {:desc "The Holy Doge Temple, here rest the sword known to hold mysterious power"
                :title "Sacred Doge Temple"
                :dir {:north :Wow}
                :contents #[Doge-Sword]}
   :Shibe-Alliance {:desc "Shibe Alliance main resistance Camp"
                :title "Shibe Alliance"
                :dir {:west :Much-Ruins}
                :contents #[]}
   :Path-of-Wow {:desc "You will protect the sacred village of Doge and all the Doge within it"
                :title "Path of Wow"
                :dir {:north :Shibe-Alliance}
                :contents #[]}
   :Forest-of-Nope {:desc "The path of blood calls to you"
                :title "Forest of NOPE"
                :dir {:west :Shibe-Alliance}
                :contents #[]}
   :So-Unknown {:desc "The path of the Doge is one must take by ones own choice"
                :title "So Unknown"
                :dir {:south :Shibe-Alliance}
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
