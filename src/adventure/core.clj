(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
			 [clojure.set :as set])
  (:gen-class))

(def the-map
  {:Harambe-Memorial {:desc "The grave of the Great Harambe. All praise Harambe! "
           :title "Great Harambe Memorial"
           :dir {:south :Such-Path}
           :contents #{}}
   :Such-Path {:desc "The main path of Doge Island."
              :title "Such Path"
              :dir {:north :Harambe-Memorial
					:south :Doge-Village
					:east :Wow-Forest
					:west :Much-Ruins}
               :contents #{:Doge-Treat}}
   :Wow-Forest {:desc "The local forest of the Doge. Rumors say a holy Doge Sword is deep within the forest"
                :title "Wow Forst"
                :dir {:west :Such-Path
					  :east :Very-Close
					  :north :So-Lost}
                :contents #{}}
   :Doge-Village {:desc "The Doge Village where all doge live and prosper"
                :title "Doge Village"
                :dir {:north :Such-Path}
                :contents #{:Doge-Shield}}
   :Much-Ruins {:desc "Ruins of an Ancient city. There stands a wall with a missing jewel in the middle."
                :title "Much Ruins"
                :dir {:east :Such-Path
					  }
                :contents #{:Doge-Coin}}
   :So-Lost {:desc "Such forest, very lost, Wow"
                :title "Wow Forest:So Lost"
                :dir {:south :Wow-Forest}
                :contents #{:Doge-Toy}}
   :Very-Close {:desc "Ruins of an Ancient city. There stands a wall with a missing jew in the middle."
                :title "Wow Forest:Very Close"
                :dir {:west :Wow-Forest
					  :north :Not-Right}
                :contents #{}}
   :Not-Right {:desc "Deep within the Wow Forest. You can see a bright light towards the right side of the forest"
                :title "Wow Forest: Definitely Not Right"
                :dir {:south :Very-Close
					  :east :Nope-Path
					  :north :Wow}
                :contents #{}}
   :Wow {:desc "Doge Temple is seen from here. Just north of Wow Forest: Wow"
                :title "Wow Forest: Wow"
                :dir {:south :Not-Right
					  :north :Doge-Temple}
                :contents #{}}
   :Nope-Path {:desc "Nope definitely not here"
                :title "Nope Path"
                :dir {:west :Not-Right}
                :contents #{:Mysterious-Crystal}}
   :Doge-Temple {:desc "The Holy Doge Temple, here rest the sword known to hold mysterious power"
                :title "Sacred Doge Temple"
                :dir {:south :Wow}
                :contents #{:Doge-Sword}}
   :Shibe-Alliance {:desc "Shibe Alliance main resistance Camp"
                :title "Shibe Alliance"
                :dir {:east :Much-Ruins
					  :west :Forest-of-Nope
					  :north :Path-of-Wow
					  :south :So-Unknown}
                :contents #{:Doge-Tag}}
   :Path-of-Wow {:desc "You will protect the sacred village of Doge and all the Doge within it"
                :title "Path of Wow"
                :dir {:south :Shibe-Alliance}
                :contents #{:Light-Essence}}
   :Forest-of-Nope {:desc "The path of blood calls to you"
                :title "Forest of NOPE"
                :dir {:east :Shibe-Alliance}
                :contents #{:Dark-Essence}}
   :So-Unknown {:desc "The path of the Doge is one must take by ones own choice"
                :title "So Unknown"
                :dir {:north :Shibe-Alliance}
                :contents #{:Brave-Essence}}
   })

(def adventurer
  {:location :Doge-Village
   :bag #{}
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
	(if (empty? (player :bag))
		(println "Have not found anything yet."))
    (apply println (player :bag)player)
	(let [location (player :location)]
     (assoc-in player [:location] location)))
		  
(defn talk [player]
(let [location(player :location)]
(println "~~~~~~~~")
	(cond
		(= location :Doge-Village)
			(do
			   (println "Store Keeper: Trade you a Doge Shield for a Doge Coin and one of them fun Doge Toys")
				(if (and (contains? (player :bag) :Doge-Toy)(contains? (player :bag) :Doge-Coin))
				(do 
				(println "Store Keeper:OH YES! Here a Doge Shield. let me know if you find more")
				(-> player (update-in [:bag] #(disj % :Doge-Toy))
							(update-in [:bag] #(disj % :Doge-Coin))
						  (update-in [:bag] #(conj % :Doge-Shield))
						  ))))
		(= location :Harambe-Memorial)
			(println "Sage: Go on forth and join the Shibe Alliance in battle. Prepare yourself by collecting the Doge Shield and Doge Sword. The holy sword is to be found deep in the forsest. Good Luck!")
			)
	(do (println "~~~~~~~~")
               player)))

(defn close []
   (println "See you next time fellow Doge.")
   (System/exit 0))

(defn pickup [player]
	(let [location(player :location)]
		(cond
		(= location :Such-Path)
			 (if (not (contains? (player :bag) :Doge-Treat))
				(do (println "A doge treat!")
					(update-in player [:bag] #(conj % :Doge-Treat))
					))
		(= location :So-Lost)
			 (if (not (contains? (player :bag) :Doge-Toy))
				(do (println "A doge toy!")
					(update-in player [:bag] #(conj % :Doge-Toy))
					))
		(= location :Nope-Path)
			 (if (not (contains? (player :bag) :Crystal))
				(do (println "Some type of crystal jewel")
					(update-in player [:bag] #(conj % :Crystal))
					))
		(= location :Much-Ruins)
			 (if (not (contains? (player :bag) :Doge-Coin))
				(do (println "Got me some cash")
					(update-in player [:bag] #(conj % :Dog-Coin))
					))
		(= location :Shibe-Alliance)
			 (if (not (contains? (player :bag) :Doge-Tag))
				(do (println "A fallen brothers doge tag....")
					(update-in player [:bag] #(conj % :Doge-Tag))
					))
		(= location :Doge-Temple)
			(do 
				(println "The Sacred temple of the Doge. I feel so empowered")
				(if(not (contains? (player :bag) :Doge-Sword))
				(do 
				(-> player (assoc-in [:location] :location)
				(update-in [:bag] #(conj % :Doge-Sword)))
				(println "The holy Doge Sword is Mine! Time to help my fellow Doge!"))
				)
				)
				)
		(do (println "Thats all...")
               player)
			   ))
						  
(defn commands [player]
  (do (println "Command list \n look \n pickup \n talk \n commands \n items \n north \n south \n east \n west ")
    )player)
(defn think [player]
(do(if (not (contains? (player :bag) :Doge-Sword))
(println "I need a sword,a shield, and the missing jewel"))
  (do (println "I am so lost")
    )player))
(defn advice [player]
  (do (println "Look for items you can trade or explore the surrounding areas!")
    )player)
(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
		 [:pickup] (pickup player)
		 [:commands] (commands player)
		 [:advice] (advice player)
		[:think] (think player)
		 [:talk] (talk player)
		 [:items](items player)
         [:north] (go :north player)
         [:south] (go :south player)
		 [:east] (go :east player)
		 [:west] (go :west player)
		 [:close] (close)

         _ (do (println "I don't understand you, but maybe I can help in events")
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
			(if (contains? (local-player :bag) :Light-Essence)
			(do (println "Thanks for Playing")
			(System/exit 0)))
			(if (contains? (local-player :bag) :Dark-Essence)
			(do (println "Thanks for Playing")
			(System/exit 0)))
			(if (contains? (local-player :bag) :Brave-Essence)
			(do (println "Thanks for Playing")
			(System/exit 0)))
			(if(and (contains? (local-player :bag) :Crystal)(= location :Much-Ruins))
		(do 
			(assoc-in local-player [:location] :Shibe-Alliance)
			(println "Where am I")
			))
		(if (= location :Path-of-Wow)
		(do (println "you have chosen the path of light good luck!")
		(update-in local-player [:bag] #(conj % :Light-Essence))
			))
			(if (= location :Forest-of-Nope)
		(do (println "you have chosen the path of darkness good luck!")
		(update-in local-player [:bag] #(conj % :Dark-Essence))
			))
			(if (= location :Path-of-Wow)
		(do (println "True power is hidden in the balance of dark and light. You have chosen your own path!")
		(update-in local-player [:bag] #(conj % :Brave-Essence))
			))
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command)))))))
