(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:Harambe-Memorial {:desc "The grave of the Great Harambe. All praise Harambe! "
           :title "Great Harambe Memorial"
           :dir {:north :Such-Path}
           :contents #[]}
   :Such-Path {:desc "The main path of Doge Island."
              :title "Such Path"
              :dir {:south :Harambe-Memorial
					:north :Doge-Village
					:west :Wow-Forest
					:east :Much-Ruins}
               :contents #[Doge-Treat]}
   :Wow-Forest {:desc "The local forest of the Doge. Rumors say a holy Doge Sword is deep within the forest"
                :title "Wow Forst"
                :dir {:east :Such-Path
					  :west :Very-Close
					  :south :So-Lost}
                :contents #[]}
   :Doge-Village {:desc "The Doge Village where all doge live and prosper"
                :title "Doge Village"
                :dir {:south :Such-Path}
                :contents #[Doge-Shield]}
   :Much-Ruins {:desc "Ruins of an Ancient city. There stands a wall with a missing jew in the middle."
                :title "Much Ruins"
                :dir {:west :Such-Path
					  :east :Shibe-Alliance}
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
					  :west :Nope-Path}
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
                :contents #[Doge-Tag]}
   :Path-of-Wow {:desc "You will protect the sacred village of Doge and all the Doge within it"
                :title "Path of Wow"
                :dir {:north :Shibe-Alliance}
                :contents #[Light-Essence]}
   :Forest-of-Nope {:desc "The path of blood calls to you"
                :title "Forest of NOPE"
                :dir {:west :Shibe-Alliance}
                :contents #[Dark-Essence]}
   :So-Unknown {:desc "The path of the Doge is one must take by ones own choice"
                :title "So Unknown"
                :dir {:south :Shibe-Alliance}
                :contents #[Colorful-Essence]}
   })

(def adventurer
  {:location : Doge-Village
   :inventory #{}
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

(defn talk [player]
(let [location](player :location)
	(cond
		(=location Doge-Village)
			(do
			   (Println "Store Keeper: Trade you a Doge Shield for a one of them fun Doge Toys")
				(if(contains? (player :inventory):Doge-Toy)
				(do 
				(Println "Store Keeper: OH YES! Here a Doge Shield. let me know if you find more")
				(->player (assoc-in [:location] location)
						  (update-in [:inventory] #(conj % :Doge-Shield))
						  )))))))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
		 [:pick-up] (pick-up player)
		 [:talk] (talk player)
         [:north] (go :north player)
         [:south] (go :south player)
		 [:east] (go :east player)
		 [:west] (go :west player)
		 [:grab] (go :grab player)
		 [:exit] (exit)

         _ (do (println "I don't understand you.")
               player)

         )) 
(defn exit []
   (println "Bye.")
   (System/exit 0))

(defn -main
  [& args]
  (Println "Welcome to the Country of Doge!")
  (Println "War is upon the Country of Doge as Harambe has been assassinated")
  (Println "Head to Harambe Memorial to see what the sages have to say")
  (loop [local-map the-map
         local-player adventurer]
	(let [location(local-player :location)]
		(if(= location :Shibe-Alliance)
		(do 
			(Println "The united Shibe forces camp seems to be in bad shape good thing I have come prepared")
			))
		(if(= location :Harambe-Memorial)
			(do 
				(Println "Sage: Go on forth and join the Shibe Alliance in battle. Prepare yourself by collecting the Doge Shield and Doge Sword. The holy sword is to be found deep in the forsest. Good Luck!")
				))
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command)))))))
