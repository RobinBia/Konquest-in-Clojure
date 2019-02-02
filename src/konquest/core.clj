(ns konquest.core
(:import (java.awt Graphics Graphics2D Color Dimension Font)
           (javax.swing JPanel JLabel JButton)
           (javax.swing.border LineBorder))
(:require [clojure.math.numeric-tower :as math]))


;(use 'konquest.core :reload-all)
;(map #(ns-unmap *ns* %) (keys (ns-interns *ns*)))
;_____________________________________________________________________________________________

(defn nat
  "String s enthält eine natürliche Zahl => Übersetzung nach Integer, ansonsten false."
  [s]
   (let [x (if (= s "") -1 (read-string s))]
      (if (and (integer? x) (> x -1)) x false)
   )
)
;(nat "") ;false
;(nat "1.0") ;false
;(nat "-1.0") ;false
;(nat "0") 0
;(nat "1"); 1
;_________________________________Hilfsfunktionen_____________________________________________

(defn pprint
  "Druckt einen Spielstand 'm' leserlich aus."
  [m]
  (print "Spielerliste: " (first m))
  (print "\nSchiffsbewegungen: ")
  (doseq [x (second m)]
    (print "\n" x)
  ) 
  (print "\nFeldinformationen: ")
  (doseq [y (into (sorted-map) (get m 2))]
    (print "\n" y)
  )  
  (print "\nSpielfeldimensionen: ")   
    (print "\n" (get-in m [3 0]) "Zeilen")
    (print "\n" (get-in m [3 1]) "Spalten")
)
;_____________________________________________________________________________________________
;Beispiel für einen Spielstand, Vektor (m) mit 4 Elementen:
;0 (a): Namensliste mit Spielern, erstes Element ist aktueller Spieler
;1 (b): Schiffsbewegungen
;2 (c): Map, die jedem Spielfeld eine Zahl zuordnet und diese auf sämtliche Informationen abbildet
;3 (d): Dimensionen des gesamten Spielfeldes
(def muster [[:A :B] [] {8 nil, 7 nil, 
                         6 {:Spieler :A, :Kill 1.1, :Schiffe 10}, 
                         5 {:Spieler :N, :Kill 1.5,:Schiffe 0}, 
                         4 {:Spieler :B, :Kill 1.8, :Schiffe 10}, 
                         3 {:Spieler :N, :Kill 0.7, :Schiffe 0},
                         2 nil, 1 nil, 0 nil}
                         [3 3]])
;_____________________________________________________________________________________________
(defn round
  "Runde die Zahl 'z' auf 'stellen' Nachkommastellen genau."
  [stellen z]
  (let [factor (Math/pow 10 stellen)]
    (/ (Math/round (* z factor)) factor))
)
;(round 3 1.0111)
;_____________________________________________________________________________________________
(defn erstelle-spielfeld 
  "Erstelle eine Informationstabelle (Hashmap) mit x*y Einträgen, wobei 'd' ein Vektor [x y] ist, 
   der die Dimension des Spielfeldes beschreibt. Alle Einträge werden auf nil abgebildet."
  [d]
  (loop [m {} x (dec (* (first d) (second d)))]
    (cond (>= x 0) 
          (recur (merge m {x nil}) (dec x)) 
          :else m
    )
  )
)
;(erstelle-spielfeld [3 3])
;_____________________________________________________________________________________________
(defn finde-freies-feld
  "Finde in Spielfeld 'c' ein Feld, das nicht belegt, also mit index |-> nil"
  [c]
   (loop [i (range 0 (count c)) n (rand-nth i)]
     (if (nil? (get c n))
          n
          (recur i (rand-nth i))
     )
   )
)
;(finde-freies-feld {0 nil 1 10 2 nil 3 11}) ; 0 und 2 werden zufällig ausgegeben
;_____________________________________________________________________________________________
(defn init-neutrale-planeten 
  "Füge für ein Spielfeld 'c0' 'anzahl' neutrale 
   Planeten hinzu und gebe neues Spielfeld zurück."
  [anzahl c0]
  (loop [i anzahl c c0]
    (cond
      (> i 0)
      (recur (dec i)
             (assoc c (finde-freies-feld c) {:Spieler :N
                                             :Kill (round 1 (rand-nth (range 0.5 2 0.1)))
                                             :Schiffe 0})
      )
      :else c
     )
  )  
)
;(init-neutrale-planeten 5 {1 nil 2 nil 3 nil 4 nil 5 nil 6 nil 7 nil 8 nil 9 nil})
;_____________________________________________________________________________________________
(defn init-hauptplanet 
  "Fügt für alle Spieler in 'c' im Spielfeld 'a0' einen Hauptplaneten hinzu.
   Gibt ein neues Spielfeld zurück."
  [a0 c0]
  (loop [a a0 c c0]
    (cond
      (not (empty? a))
      (recur (subvec a 1)
          (assoc c (finde-freies-feld c) {:Spieler (first a) 
                                          :Kill (round 1 (rand-nth (range 0.5 2 0.1)))                                          :Schiffe 10})
      )
      :else c
     )
  )
)
;(init-hauptplanet [:A :B] {0 nil 1 nil 2 nil 3 nil})
;_____________________________________________________________________________________________
(defn spieler-wechseln
  "Hängt für einen Spielstand 'm' den vordersten Spieler aus 'a' (get m 0) 
   hinten an."
  [m]
  (let [spieler (get m 0)]
   (->>
      (conj (subvec spieler 1) (first spieler))
      (assoc-in m [0] ,,,)
   )
  )
)
;(spieler-wechseln muster) ;Wird immer benutzt, wenn end-turn! aufgerufen wird.
;_____________________________________________________________________________________________
(defn transform-in-2D 
  "Berechnet die Koordinaten eines Array-Elements 
   an Index 'x' für eine Matrix mit Dimension 'dim', worbei card(dim)=2"
  [x dim]
  (loop [j 0 i 0 k 0]
      (cond
           (= x k)
             [i j]  
           (and (= j (dec (second dim))) (< i (dec (first dim))))
                      (recur 0 (inc i) (inc k))
           (and (< j (dec (second dim))) (<= i (first dim)))
                      (recur (inc j) i (inc k))
           :else nil
      )
  )
)
;Wo liegt das 5. Element (Index+1) in der Matrix?
;(transform-in-2D 4 [4 3])
;4 [3 4] -> [1 0]
;4 [4 3] -> [1 1]
;Wo liegt das 4. Element in der Matrix?
;(transform-in-2D 3 [3 4])
;_____________________________________________________________________________________________

;und schneidet die Nachkommastellen ab. Ergebnis ist die Anzahl
;der Runden, die für das Zurücklegen der Strecke zwischen Punkt (Planeten) p1 und p2 benötigt wird.
(defn distanz 
  "Berechnet ganzzahlige Distanz zwischen 2 Feldpositionen (1D) für eine Matrix mit Dimension 'dim'."
  [von nach dim]
  (let [von (transform-in-2D von dim) nach (transform-in-2D nach dim)]
		(->>
		  (let [x (into [] (for [x von y nach](- x y)))]
		    (->>
		     (+ (* (first x) (first x)) (* (last x) (last x)))
		     (math/sqrt)
		    )
		  )
		  (int ,,,)
		)
  )
)
;(distanz 0 7 [3 4]) 
;(distanz 0 7 [4 3]) 
;_____________________________________________________________________________________________
(defn ausreichend-felder?
  "Ermittelt ob es in einem Spielfeld mit Dimension 'dim' für alle Spieler 
   in 'a' (Spielerliste) und 'anzahl-NP' neutrale Planeten genügend Felder gibt"
  [dim a anzahl-NP]
  (if (<= (+ (count a) anzahl-NP) (* (first dim) (second dim)))
     true 
     false
  )
)
;(ausreichend-felder? [3 3] ["" ""] 7) ;-> true
;(ausreichend-felder? [3 3] ["" ""] 8) ;-> false 
;_____________________________________________________________________________________________
(defn erzeuge-spiel 
  "Erzeuge ein Spiel 'm' mit einem Spielfeld der Dimension 'dim', der Spielerliste 'a'
   und 'anzahl-NP' neutralen Planeten, sofern das Spielfeld groß genug ist. Ansonsten nil."
  [dim a anzahl-NP]
  (if (ausreichend-felder? dim a anzahl-NP)
    (into
     (->> 
	      (erstelle-spielfeld dim)
	      (init-hauptplanet a ,,,)
        (init-neutrale-planeten anzahl-NP ,,,)
        (vector a [] ,,,)
     ) [dim])
     (do (print "Die Anzahl der Planeten ist größer als das Spielfeldraster!") nil)
  )
)
;(pprint (erzeuge-spiel [4 3] [:A :B] 4))
;(erzeuge-spiel [3 3] [:A :B] 8) ;-> nil
;_____________________________________________________________________________________________
(defn am-zug 
  "Gibt aus, welcher Spieler laut Spielstand 'm' an der Reihe ist"
  [m] 
  (get-in m [0 0])
) 
;(am-zug (erzeuge-spiel [3 3] [:A :B] 2)) ;-> :A
;_____________________________________________________________________________________________
(defn schiffe-senden 
  "Sendet für einen Spielstand 'm' 'anzahl' Schiffe, von  'von' nach 'nach', sofern dies
   erlaubt und gibt einen neuen Spielstand zurück. Wenn der Zug nicht erlaubt ist, wird 
   der alte Spielstand zurückgegeben und eine Fehlermeldung ausgegeben"
  [von nach anzahl m]
  (cond
    (= anzahl 0)
        (do (print "Sie besitzen leider keine nichtexistenten Schiffe") m)
    (= (get-in m [2 von]) nil) 
        (do (print "Bitte wählen Sie einen Planeten aus, um von diesem Ihre Schiffe zu verschicken!") m)
    (= (get-in m [2 nach]) nil) 
        (do (print "Schiffe können nur verschickt werden, wenn das Ziel ein Planet ist!") m)
    (not= (am-zug m) (get-in m [2 von :Spieler]))
        (do (print "Sie können keine Schiffe von einem gegnerischen Planeten verschicken!") m)
    (> anzahl (get-in m [2 von :Schiffe]))
        (do (print "Sie können nicht mehr Schiffe verschicken, als Sie besitzen!") m)
    :else
        (-> 
             (update-in m [2 von :Schiffe] - anzahl) ; Schiffe von Startplaneten abziehen
             (update-in ,,, [1] conj {:Spieler (am-zug m) 
                                      :Anzahl anzahl
                                      :Start von
                                      :Ziel nach 
                                      :Entfernung (distanz von nach (get-in m [3]))})
        )
  )
)
;Schicke 5 Schiffe von dem aktuell Spieler :A gehörenden Planeten 0 zum Planeten 2.
;(schiffe-senden 6 0 5 muster) ;Schiffe können nur verschickt werden, wenn das Ziel ein Planet ist!
;(schiffe-senden 0 4 5 muster) ;Bitte wählen Sie einen Planeten aus, um von diesem Ihre Schiffe zu verschicken!
;(schiffe-senden 5 4 5 muster) ;Sie können keine Schiffe von einem gegnerischen Planeten verschicken!
;(schiffe-senden 5 4 5 muster) ;Sie können keine Schiffe von einem gegnerischen Planeten verschicken!
;(schiffe-senden 6 5 11 muster) ;Sie können nicht mehr Schiffe verschicken, als Sie besitzen!
;(schiffe-senden 6 5 5 muster) 
;_____________________________________________________________________________________________
(defn bewegungen 
  "Zeigt für einen Spielstand 'm' die Schiffsbewegungen 'b' des aktuellen Spielers 'a0' an."
  [m]
  (for [x (get-in m [1]) 
        :when (= (get x :Spieler) (am-zug m))
       ]
    x
  )
)
;(bewegungen (schiffe-senden 6 5 5 muster))
;_____________________________________________________________________________________________

(defn sichern-nach 
  "Spielstand 'm' in einer Datei 'pfad' als String sichern."
  [pfad m] 
  (spit pfad  m))
;(sichern-nach "savegame.temp" (schiffe-senden 6 5 5 muster))
;_____________________________________________________________________________________________
(defn laden-von 
  "Spielstand 'm' aus einer Datei 'pfad' lesen."
  [pfad] 
  (read-string  (slurp pfad)))
;(laden-von "savegame.temp")
;_____________________________________________________________________________________________
;Dekrementiert die Entfernung aller Schiffe von ihrem Zielplanet um 1
(defn update-entfernung 
  "Dekrementiert für den Spiel 'm0' die Entfernung aller Schiffe 'b' 
   und gibt neuen Spielstand zurück."
   [m0]
   (loop [m m0 i (dec (count (get-in m [1])))]
     (if (>= i 0) 
	      (recur (update-in m [1 i :Entfernung] - 1) (dec i))
	      m           
      )      
   )
)
;(update-entfernung [[] [{:Entfernung 5} {:Entfernung 4}]])
;_____________________________________________________________________________________________
(defn schiffsverlust 
 "Berechnet übrigbleibende Schiffe aus Angriffskraft 'atk0',
  Verteidigungskraft 'ver0' und einer Anzahl von Schiffen 'n'." 
   [atk0 ver0 n]
   (let [atk (math/expt atk0 3) ;Kubischer Zusammenhang zwischen Schiffsverlust und Kampfausgeglichenheit
         ver (math/expt ver0 3)
         p (/ (Math/abs (- atk ver)) (+ atk ver))]        
	     (if (>= p 1) 
	       1 
	       (->>
	         (int (* p n))
	         (max 1 ,,,)
	       )
	     )
	 )
)
;(schiffsverlust 110 120 120) ;-> 15 Ausgeglichen => Fast alles zerstört
;(schiffsverlust 30 120 120)  ;-> 116 Unausgeglichen => Fast nichts zerstört
;(schiffsverlust 60 120 120)  ;-> 93 Einer hat doppelt so viel wie der andere => leichter Verlust
;_____________________________________________________________________________________________
;Kampf zwischen 2 Parteien
;Berechne Gesamtverteidigungstärke des betrachteten Planeten und
;Gesamtangriffsstärke des Angreifers.
;Aktualisiert den Gewinner und passt die Anzahl der erhaltenen Schiffe an.

(defn kampf2p [v_i start ziel m]
 (let [name_angreifer       (get-in v_i [:Spieler])
       anzahl_angreifer     (get-in v_i [:Anzahl])
       anzahl_verteidiger   (get-in m [2 ziel :Schiffe])
       kill_angreifer       (get-in m [2 start :Kill])
       kill_verteidiger     (get-in m [2 ziel :Kill])
       angriff (* anzahl_angreifer kill_angreifer)                  
       verteidigung (* anzahl_verteidiger kill_verteidiger 1.1)] ;10% Bonus auf die Verteidigung
    (cond
     (= anzahl_verteidiger 0) ;Dies gilt für alle neutralen Planeten. Hier findet kein Kampf statt!
       (-> 
          (assoc-in m [2 ziel :Schiffe] anzahl_angreifer)
          (assoc-in ,,, [2 ziel :Spieler] name_angreifer)
       )
      (> angriff verteidigung) ;Nimm Planeten ein, aber verliere Schiffe
       (->
           (assoc-in m [2 ziel :Spieler] name_angreifer)
           (assoc-in ,,, [2 ziel :Schiffe] anzahl_angreifer)
           (assoc-in ,,, [2 ziel :Schiffe] (schiffsverlust angriff verteidigung anzahl_angreifer))
        )
      (<= angriff verteidigung) ;Wehre Angreifer erfolgreich ab, aber verliere Schiffe
         (assoc-in m [2 ziel :Schiffe] (schiffsverlust angriff verteidigung anzahl_verteidiger))        
      :else nil     
    )    
 )    
)
;_____________________________________________________________________________________________
;Berechnet einen Vektor mit allen Schiffsbewegungen, die in diesem Zug zu 
;einem Ziel gekommen sind
(defn kampfbereit 
  "Extrahiert aus einem Spiel 'm' alle Schiffsbewegungen mit folgenden Eigenschaften:
   Entfernung ist 0; Das Ziel ist ein Planet, der einem gegnerischen Spieler gehört."
  [m]
  (into [] 
    (for [x (range 0 (count (get-in m [1])))
          :when (and (= 0 (get-in m [1 x :Entfernung])); Schiffe sind am Ziel
                     (not= (get-in m [1 x :Spieler]) 
                           (get-in m [2 (get-in m [1 x :Ziel]) :Spieler])))] ;Schiffe sind gegnerisch          
         (get-in m [1 x])
         
    )
  )
)
;(kampfbereit (update-entfernung (schiffe-senden 4 6 10 (zug-beenden muster))))
;-> [{:Spieler :B, :Anzahl 10, :Start 4, :Ziel 6, :Entfernung 0}]
;(kampfbereit (update-entfernung (update-entfernung (schiffe-senden 6 5 10 muster))))
;-> [{:Spieler :A, :Anzahl 10, :Start 6, :Ziel 5, :Entfernung 0}]
;_____________________________________________________________________________________________

(defn stationierbar 
  "Extrahiert aus einem Spiel 'm' alle Schiffsbewegungen mit folgenden Eigenschaften:
   Entfernung ist 0; Das Ziel ist ein Planet, der dem auftraggebendem Spieler gehört."
  [m]
  (into [] 
    (for [x (range 0 (count (get-in m [1])))
          :when (and (= 0 (get-in m [1 x :Entfernung])); Schiffe sind am Ziel
                     (= (get-in m [1 x :Spieler]) 
                           (get-in m [2 (get-in m [1 x :Ziel]) :Spieler])))] ;Schiffe gehören diesem Spieler        
         (get-in m [1 x])       
    )
  )
)
;(stationierbar
;(update-entfernung (update-entfernung 
;(schiffe-senden 5 6 5 (zug-beenden (zug-beenden (schiffe-senden 6 5 10 muster)))))))
;->[{:Spieler :A, :Anzahl 5, :Start 5, :Ziel 6, :Entfernung 0}]
;_____________________________________________________________________________________________
;Berechnet den Ausgang aller stattfindenden Kämpfe
(defn kämpfen 
  "Berechnet für ein Spiel 'm0' für alle kampfbereiten Schiffe 'k0' 
   den Ausgang eines 2-Parteien-Kampfes und gibt einen neuen Spielstand zurück."
  [m0 k0]
  (loop [m m0 k k0]
     (if (empty? k)
        m
       (recur (kampf2p (first k) (get-in k [0 :Start]) (get-in k [0 :Ziel]) m) (subvec k 1))
     )
  )
)

;(kämpfen [[] [] {0 {:Spieler :A :Schiffe 5 :Kill 2} 1 {:Spieler :B :Schiffe 10 :Kill 1}} []]
;          [{:Spieler :A :Start 0 :Ziel 1 :Anzahl 100 :Entfernung 0}])
;->[[] [] {0 {:Spieler :A, :Schiffe 5, :Kill 2}, 1 {:Spieler :A, :Schiffe 99, :Kill 1}} []]
;_____________________________________________________________________________________________
(defn stationieren 
  "Berechnet für ein Spiel 'm0' für alle stationierbaren Schiffe 's0' 
   eine neue Schiffsanzahl beim Zielplaneten und gibt den neuen Spielstand zurück."  
  [m0 s0]
  (loop [m m0 s s0]
    (if (empty? s)
       m
      (recur (update-in m [2 (get-in s [0 :Ziel]) :Schiffe] + (get-in s [0 :Anzahl])) (subvec s 1))
    )
  )
)
;(stationieren [[] [] {0 {:Spieler :A :Schiffe 5 :Kill 2} 1 {:Spieler :A :Schiffe 10 :Kill 1}} []]
;          [{:Spieler :A :Start 0 :Ziel 1 :Anzahl 100 :Entfernung 0}])
;-> [[] [] {0 {:Spieler :A, :Schiffe 5, :Kill 2}, 1 {:Spieler :A, :Schiffe 110, :Kill 1}} []]
;_____________________________________________________________________________________________
(defn auswerten 
  "Berechnet für ein Spiel 'm0' den neuen Spielstand nach dem Stationieren und Kämpfen."
  [m0]
   (let [m1 (stationieren m0 (stationierbar m0))] ;Zuerst stationieren 
       (kämpfen m1 (kampfbereit m1)) ;Anschließend kämpfen      
   )
)  
;(auswerten [[] [{:Spieler :A :Start 0 :Ziel 1 :Anzahl 100 :Entfernung 0}
;                {:Spieler :B :Start 2 :Ziel 3 :Anzahl 100 :Entfernung 0}] 
;                {0 {:Spieler :A :Schiffe 10 :Kill 2} 
;                 1 {:Spieler :A :Schiffe 10 :Kill 1}
;                 2 {:Spieler :B :Schiffe 10 :Kill 1}
;                 3 {:Spieler :N :Schiffe 10 :Kill 1}}[]])

;->[[] [...] {0 {:Spieler :A, :Schiffe 10, :Kill 2}, 
;             1 {:Spieler :A, :Schiffe 110, :Kill 1}, 
;             2 {:Spieler :B, :Schiffe 10, :Kill 1}, 
;             3 {:Spieler :B, :Schiffe 99, :Kill 1}} []]
;_____________________________________________________________________________________________
(defn entferne-schiffsbewegungen 
  "Alle Schiffsbewegungen 'b' in dem Spiel 'm' mit der Entfernung 0 werden entfernt"
  [m]
  (->>
	    (for [x (get m 1)
	          :when (not= (get-in x [:Entfernung]) 0)]
	          x
	    ) 
      (into [] ,,,)
      (assoc-in m [1] ,,,)
  )
)
;(entferne-schiffsbewegungen [[] [{:Entfernung 0} {:Entfernung 1}]]) ;-> [[] [{:Entfernung 1}]]
;_____________________________________________________________________________________________

(defn schiffsreproduktion 
  "Fügt für das Spiel m0 allen neutralen Planeten 1-3 Schiffe hinzu und
   allen durch Spieler besetzten Planeten 4-8 Schiffe."
  [m0]
  (loop [m m0 x 0]
     (cond
          (> x (dec (count (get m 2)))) 
                  m
          (nil? (get-in m [2 x])) 
                 (recur m (inc x)) 
          (= (get-in m [2 x :Spieler]) :N) 
                 (recur (update-in m [2 x :Schiffe] + (rand-nth (range 1 3))) (inc x))
          :else 
                 (recur (update-in m [2 x :Schiffe] + (rand-nth (range 4 8))) (inc x))
     )
  )
)
; (schiffsreproduktion [[] [] [{:Spieler :A :Schiffe 3} {:Spieler :B :Schiffe 2}]])
;->[[] [] [{:Spieler :A, :Schiffe 8} {:Spieler :B, :Schiffe 9}]]
;_____________________________________________________________________________________________
(defn gewinner
  "Ermittelt aus Spiel 'm' einen Gewinner - Bedingung: card(c\\{:N,nil}) = 1.
   Gibt Namen des Gewinners aus, wenn Bedingung erfüllt, ansonsten nil."
 [m]
 (let [spieler (->> (for [x (get-in m [2])] (get-in x [1 :Spieler])) 
                    (remove nil? ,,,) 
                    (distinct ,,,)
                    (filter (fn [n] (if (= n :N) false true)) ,,,))]
     (if (> (count spieler) 1)
         nil
         (first spieler)
     )  
 )
)
;(gewinner [[] [] {0 {:Spieler :Robin} 1 {:Spieler :Robin}}]) -> :Robin
;_____________________________________________________________________________________________
(defn zug-beenden 
 "Beendet Spielzug bezüglich 'm' dh. es werden alle Schiffsbewegungen und Kämpfe 
  ausgewertet und anschließend ein neuer Spielstand zurückgegeben."
 [m]
 (let [m1 (->>   
            (update-entfernung m) ;Dekrementiert die Entfernung aller Schiffe von ihrem Zielplanet um 1
            (auswerten ,,,) ;Ermittelt alle ankommenden Schiffe und wertet aus, ob gekämpft oder stationiert wird
            (entferne-schiffsbewegungen ,,,) ;Löscht alle Schiffe aus den Bewegungen, die an einem Kampf beteiligt waren
            (schiffsreproduktion ,,,) ;Erhöht die Schiffsanzahl auf besetzten Planeten stärker, auf Neutralen weniger
            (spieler-wechseln ,,,)) ;Schreibt den vordersten Spieler aus 'a' nach ganz hinten
        win (gewinner m1)] ;Ermittelt einen eventuellen Gewinner
     (if (nil? win) ;Wenn Gewinner gefunden werden kann, erfolgt eine Ausgabe,
            m1 
            (do (println (str "Spieler " win " hat das Spiel gewonnen !")) m1)
     )
 )
)

;(zug-beenden (schiffe-senden 6 5 10 muster))
;->
;[[:B :A] [{:Spieler :A, :Anzahl 10, :Start 6, :Ziel 5, :Entfernung 1}] 
; {[...] 4 {:Spieler :B, :Kill 1.8, :Schiffe 15}, 6 {:Spieler :A, :Kill 1.1, :Schiffe 6}, [..]} [3 3]]
;_____________________________________________________________________________________________

(defn berechne-abstandhalter 
   "Berechnet für eine spezifisch anzuzeigende Information aus 'c' 
    einen leeren String, dessen Länge addiert auf die Stringlänge der Information
    Konstant ist."
   [ci]
   (apply str (repeat 
    (->>
           (if (nil? (get ci 1))
               "  -  "
               [(get-in ci [1 :Spieler]) "," (get-in ci [1 :Schiffe])]
           )
           (apply str ,,,)
           (count ,,,)
           (- 20 ,,,)
     ) " "))
)
;(berechne-abstandhalter [{:Spieler :A :Schiffe 10} {:Spieler :B :Schiffe 20}])
;-> "               "
;_____________________________________________________________________________________________
(defn druck-2d 
  "Druckt einen Vektor mit n Strings in der Dimension 'dim' aus, wobei dim = [x y] und n=x*y"
  [dim v]
  (if (= (count v) (* (first dim) (second dim)))      
		  (doseq [x (range 0 (count v))]
		    (if (= (mod x (second dim)) 0)
		      (print "\n\u0009" (get-in v [x]))
		      (print (get-in v [x]))
		    )
		  )
      nil
  )
)
;(druck-2d [2 2] [1 2 3 4 5]) ;-> nil
;(druck-2d [3 4] [" 1 " " 2 " " 3 " " 4 " " 5 " " 6 " " 7 " " 8 " " 9 " " 10 " "11 " "12 "])
;->
;
;	  1  2  3  4 
;	  5  6  7  8 
;	  9  10 11 12 
;_____________________________________________________________________________________________
(defn spielfeld-zeigen
  "Druckt eine menschenlesbare Repräsentation des Spielfeldes für ein Spiel 'm' aus.
   :N    - Hier ist ein neutraler Planet.
   :A,10 - Dieser Planet gehört Spieler :A und es sind 10 Schiffe stationiert."
  [m]
  (print "Spieler" (am-zug m) "ist am Zug" "\nFlottenbewegungen:" (bewegungen m))
  (->>
        (for [ci (into (sorted-map) (get-in m [2]))]
           (if (nil? (get ci 1))
               (str "  -  " (berechne-abstandhalter ci))
               (apply str [(get-in ci [1 :Spieler])
                               "," (get-in ci [1 :Schiffe]) (berechne-abstandhalter ci)])             
           )
        )
        (vec ,,,)
        (druck-2d (get m 3) ,,,)
   )           
)

;(spielfeld-zeigen (erzeuge-spiel [4 4] ["Max" "Ursula"] 2))
;->
;
;	   -                 Ursula,10             -                   -                 
;	   -                 Max,10                -                   -                 
;	   -                   -                   -                   -                 
;	   -                 :N,0                  -                 :N,0                

