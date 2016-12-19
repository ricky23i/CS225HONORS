(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:Harambe-Memorial {:desc "The grave of the Great Harambe. All praise Harambe! "
           :title "Great Harambe Memorial"
           :dir {:south :Such-Path}
           :contents []}
   :Such-Path {:desc "The main path of Doge Island."
              :title "Such Path"
              :dir {:north :Harambe-Memorial
					:south :Doge-Village
					:east :Wow-Forest
					:west :Much-Ruins}
               :contents []}
   :Wow-Forest {:desc "The local forest of the Doge. Rumors say a holy Doge Sword is deep within the forest"
                :title "Wow Forst"
                :dir {:west :Such-Path
					  :east :Very-Close
					  :north :So-Lost}
                :contents []}
   :Doge-Village {:desc "The Doge Village where all doge live and prosper"
                :title "Doge Village"
                :dir {:north :Such-Path}
                :contents []}
   :Much-Ruins {:desc "Ruins of an Ancient city. There stands a wall with a missing jewl in the middle."
                :title "Much Ruins"
                :dir {:east :Such-Path
					  :west :Shibe-Alliance}
                :contents []}
   :So-Lost {:desc "Such forest, very lost, Wow"
                :title "Wow Forest:So Lost"
                :dir {:south :Wow-Forest}
                :contents []}
   :Very-Close {:desc "Ruins of an Ancient city. There stands a wall with a missing jew in the middle."
                :title "Wow Forest:Very Close"
                :dir {:west :Wow-Forest
					  :north :Not-Right}
                :contents []}
   :Not-Right {:desc "Deep within the Wow Forest. You can see a bright light towards the right side of the forest"
                :title "Wow Forest: Definitely Not Right"
                :dir {:south :Very-Close
					  :east :Nope-Path}
                :contents []}
   :Wow {:desc "Doge Temple is seen from here. Just north of Wow Forest: Wow"
                :title "Wow Forest: Wow"
                :dir {:south :Not-Right
					  :north :Doge-Temple}
                :contents []}
   :Nope-Path {:desc "Nope definitely not here"
                :title "Nope Path"
                :dir {:west :Not-Right}
                :contents []}
   :Doge-Temple {:desc "The Holy Doge Temple, here rest the sword known to hold mysterious power"
                :title "Sacred Doge Temple"
                :dir {:south :Wow}
                :contents [:Doge-Sword]}
   :Shibe-Alliance {:desc "Shibe Alliance main resistance Camp"
                :title "Shibe Alliance"
                :dir {:east :Much-Ruins
					  :west :Forest-of-Nope
					  :north :Path-of-Wow
					  :south :So-Unknown}
                :contents []}
   :Path-of-Wow {:desc "You will protect the sacred village of Doge and all the Doge within it"
                :title "Path of Wow"
                :dir {:south :Shibe-Alliance}
                :contents []}
   :Forest-of-Nope {:desc "The path of blood calls to you"
                :title "Forest of NOPE"
                :dir {:east :Shibe-Alliance}
                :contents []}
   :So-Unknown {:desc "The path of the Doge is one must take by ones own choice"
                :title "So Unknown"
                :dir {:north :Shibe-Alliance}
                :contents []}
   })

(def adventurer
  {:location :Doge-Village
   :inventory #{}
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (println (str "You are in " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (println (-> the-map location :desc)))
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

(defn items [player]
   (println (player :inventory))
   (let [location (player :location)]
     (assoc-in player [:location] location)))	  
	  
(defn talk [player]
(let [location(player :location)]
	(cond
		(= location :Doge-Village)
			(do
			   (println "Store Keeper: Trade you a Doge Shield for a one of them fun Doge Toys")
				(if(contains? (player :inventory):Doge-Toy)
				(do 
				(println "Store Keeper:OH YES! Here a Doge Shield. let me know if you find more")
				(-> player (assoc-in [:location] location)
						  (update-in [:inventory] #(conj % :Doge-Shield))
						  )))))
	(do (println "No one to talk to")
               player)))

(defn close []
   (println "See you next time fellow Doge.")
   (System/exit 0))

(defn pickup [player]
	(let [location(player :location)]
		(cond
		(= location :Such-Path)
			(do 
				(if(not (contains? (player :inventory):Doge-Treat))
				(-> player (assoc-in [:location] location)
						  (update-in [:inventory] #(conj % :Doge-Treat))))))
		(do (println "There seems to be nothing here")
               player)
			   ))
						  
(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
		 [:pickup] (pickup player)
		 [:talk] (talk player)
		 [:items](items player)
         [:north] (go :north player)
         [:south] (go :south player)
		 [:east] (go :east player)
		 [:west] (go :west player)
		 [:close] (close)

         _ (do (println "I don't understand you.")
               player)

         )) 

(defn -main
  [& args]
  (println "Welcome to the Country of Doge!")
  (println "War is upon the Country of Doge as Harambe has been assassinated")
  (println "Head to Harambe Memorial to see what the sages have to say")
  (loop [local-map the-map
         local-player adventurer]
	(let [location(local-player :location)]
		(if(= location :Shibe-Alliance)
		(do 
			(println "The united Shibe forces camp seems to be in bad shape good thing I have come prepared")
			))
		(if(= location :Harambe-Memorial)
			(do 
				(println "Sage: Go on forth and join the Shibe Alliance in battle. Prepare yourself by collecting the Doge Shield and Doge Sword. The holy sword is to be found deep in the forsest. Good Luck!")
				))
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command)))))))
