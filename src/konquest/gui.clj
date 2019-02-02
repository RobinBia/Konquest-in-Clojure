(ns konquest.gui
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math])
  (:import (java.awt Graphics Graphics2D Color Dimension Font 
                     BorderLayout GridLayout Toolkit Image
                     ComponentOrientation)
           (javax.swing JPanel JLabel JButton JTextPane JTextArea SpringLayout JOptionPane
                        JLayeredPane SwingConstants ImageIcon BorderFactory)
           (javax.swing.text StyleConstants SimpleAttributeSet)
           (javax.swing.border LineBorder)
           (java.awt.event ActionListener)
   )
)

(use 'konquest.core :reload-all)
(use 'konquest.gui :reload-all)
;(map #(ns-unmap *ns* %) (keys (ns-interns *ns*)))

;_____________________________________________________________________________________________
;Formatierter String für die Schiffsbewegungsanzeige
(defn pprint-schiffe 
  "Formatierungsfunktion für den Ausdruck der Schiffsbewegungen.
   Dabei ist spieler der Spieler, der am Zug ist und bew0 alle Schiffsbewegungen"
  [spieler bew0]
  (str "<html>"
		  (loop [i (first bew0) bew bew0 string "Schiffsbewegungen:<br>"]
		     (cond  (empty? bew) 
		                string
		            (= (get i :Spieler) spieler) 
		                (recur (second bew) (rest bew) (str string "Von " (get i :Start) 
		                                                            " Nach " (get i :Ziel) 
		                                                            " Anzahl " (get i :Anzahl)
		                                                            " Distanz " (get i :Entfernung) "<br>"))
		            :else 
		                (recur (second bew) (rest bew) string)          
		     )
		  )
    "</html>"
   )
)
;_____________________________________________________________________________________________
;Formatierter String für die Tooltips über den Planeten
(defn pprint-tooltip 
  "Formatierungsfunktion für Feld-Tooltips, die mit überfahren der Maus aktiviert werden.
   hm ist die Information eines Feldes und id der Index der Information (gleichzeitig Planet-ID)."
  [hm id]
  (str
       "<html><p style=font-size:140%;>"
       "Spieler:    " (get hm :Spieler) "<br>"
       "Schaden:    " (get hm :Kill)    "<br>"
       "Schiffe:    " (get hm :Schiffe) "<br>"
       "Planet-ID:  " (str id) "<br>"
       "</p><html>"
  )
)
;_____________________________________________________________________________________________
(defn get-planet-pics 
  "Erstellt eine Sequenz mit 11 Planeten-ImageIcons. Die Icons liegen in \\png
   'm' ist ein Spielstand und 'screen-size' ist die größe des Monitors (Typ Dimension)."
  [m screen-size]
  (let [max-size  (math/expt (apply max (get m 3)) (int 1.5))]
     (for [i (range 1 12)]
		       (ImageIcon. 
              (.getScaledInstance 
	                (.getImage (ImageIcon. (str "png\\p" i ".png"))) 
                  (/ (/ (. screen-size height) 2.5) max-size) 
                  (/ (/ (. screen-size height) 2.5) max-size)
                  (. java.awt.Image SCALE_SMOOTH)                   
              )                 
		       )
     )
  )
)
;_____________________________________________________________________________________________

(defn update-raster 
  "Aktualisiert das Spielfeld 'raster' und die Spieler-Anzeige 
   'spieler-label' anhand des neuen Spielstnades 'm'."
  [m raster spieler-label]
  (.setText spieler-label (str "Spieler " (get-in m [0 0]) " ist an der Reihe"))
  (loop [comps (.getComponents raster) component (first comps) i 0]
      (cond 
            (empty? comps) nil
            (nil? (get-in m [2 i])) (recur (rest comps) (second comps) (inc i))
            :else
                 (when true
                         (.setText component (str (get-in m [2 i :Spieler])))
                         (.setToolTipText component (pprint-tooltip (get-in m [2 i]) i))                 
                         (recur (rest comps) (second comps) (inc i))
                 )
      )
  ) 
)
;_____________________________________________________________________________________________

(defn init-raster 
 "Erzeugt ein leeres Spielfeldraster 'raster' andhand der Spielinformation 'm', der
  Spielfeldanzahl 'anzahl-felder' berechnet aus der Felddimension (get m 3), der 
  Bilschirmdimension 'screen-size' und der ImageIcon-Squenz mit den Planetenbildern
  'planet-pics'."
 [m raster anzahl-felder screen-size planet-pics]
 (when true
    (.setBounds raster (/ (. screen-size width) 2.5) (/ (. screen-size height) 20) ;Position
                       (/ (. screen-size width) 2) (/ (. screen-size height) 1.2)) ;Ausdehnung
    (.setLayout raster (GridLayout. (int (get-in m [3 0]))  (int (get-in m [3 1]))))
    (.setOpaque raster false)
    
    (loop [jl (JLabel.) i 0]
         (if (< i anzahl-felder)
           (when true
             (if (nil? (get-in m [2 i])) 
                nil
                (.setIcon jl (rand-nth planet-pics))
             )
			       (.setFont jl (Font. "SansSerif"  (/ (. screen-size width) 90) 
                                              (/ (. screen-size width) 90)))
			       (.setHorizontalTextPosition jl (. SwingConstants RIGHT))
			       (.setVerticalTextPosition jl (. SwingConstants BOTTOM))
			       (.setBorder jl (LineBorder. (. Color WHITE) 2))
			       (.setHorizontalAlignment jl (. SwingConstants CENTER))
			       (.setForeground jl (. Color white))
			       (.setOpaque jl true)
			       (.setBackground jl (Color. 0 0 0 128))
             (.add raster jl)
             (recur (JLabel.) (inc i))
           )
           nil
         )
    ) 
 )
)
;_____________________________________________________________________________________________
(defn init-von-nach-anzahl 
  "Erzeugt die Eingabefelder und Beschreibungen für Schiffskommandos in Abhängigkeit der 
   Bildschirmgröße 'screen-size' und fügt diese dem übergeordneten JPanel 'vna' zu."
  [vna screen-size]
   (let [von (JTextPane.) nach (JTextPane.) anzahl (JTextPane.)
         doc1 (.getStyledDocument von) 
         doc2 (.getStyledDocument nach) 
         doc3 (.getStyledDocument anzahl)
         center (SimpleAttributeSet.)
         vonl (JLabel. "Von" (. SwingConstants CENTER)) 
         nachl (JLabel. "Nach" (. SwingConstants CENTER)) 
         anzahll (JLabel. "Anzahl" (. SwingConstants CENTER))] 

     (.  StyleConstants setAlignment center (. StyleConstants ALIGN_CENTER))
     (.setParagraphAttributes doc1 0 (.getLength doc1) center false)  
     (.setParagraphAttributes doc2 0 (.getLength doc2) center false)
     (.setParagraphAttributes doc3 0 (.getLength doc3) center false)     
       
	   (.setLayout vna (GridLayout. 3 2))
     (.setOpaque vna false)
     (.add vna vonl)
	   (.add vna von)
     (.add vna nachl)
	   (.add vna nach)    
     (.add vna anzahll)
	   (.add vna anzahl)
  
     (doseq [x (.getComponents vna)]
       (.setBorder x (LineBorder. (. Color white)))
       (.setFont x (Font. "SansSerif" (/ (. screen-size width) 80) (/ (. screen-size width) 80)))
       (if (= (str (type x)) "class javax.swing.JTextPane") (.setOpaque x false) (.setOpaque x true))
			 (.setBackground x (Color. 0 0 0 128))
       (.setForeground x (. Color white))
     )
   )
   (.setBounds vna (/ (. screen-size width) 20) (/ (. screen-size height) 2) 
                   (/ (. screen-size width) 4) (/ (. screen-size height) 8))
)
;_____________________________________________________________________________________________

(defn check-senden 
  "Überprüft anhand des Spielstands 'm0' und den Schiffskommandotextfeldern in dem Jpanel
   'vna', ob alle Eingaben gültig sind. Eingaben sind nur gültig,
   wenn in jedem Textfeld eine natürliche Zahl steht."
  [vna m0]  
  (let [von (nat (.getText (.getComponent vna 1)))
        nach (nat (.getText (.getComponent vna 3)))
        anzahl (nat (.getText (.getComponent vna 5)))]
		  (cond
		      (or (= von false) 
		          (= nach false)
		          (= anzahl false))
              m0		          
		      :else 
		          (schiffe-senden von nach anzahl m0)
		  )
  )
)
;_____________________________________________________________________________________________

(defn clear-textpane 
     "Löscht den Text aus den Textfeldern des Grid-JPanels 'vna' für Schiffsbewegungsangaben."
     [vna]
     (.setText (.getComponent vna 1) "")  ;Textfeld von "Von"
     (.setText (.getComponent vna 3) "")  ;Textfeld von "Nach"
     (.setText (.getComponent vna 5) "")  ;Textfeld von "Anzahl" 
)
;_____________________________________________________________________________________________
(defn init-end-button 
  "Initialisierung des Action-Listeners für den 'Spielzug Beenden'-Button. Dieser ist abhängig von 
   sämtlichen Objekten, die dem Spielfenster zugeordnet sind."
  [screen-size layerPane raster end-button vna spieler-label schiffe-label win-label wallpaper]
	(.setFont end-button (Font. "SansSerif" (/ (. screen-size width) 90) (/ (. screen-size width) 90)))	
  (. end-button (addActionListener (proxy [ActionListener] [] (actionPerformed [evt]
        (let [m0      (laden-von "savegame.temp") 
              m1      (check-senden vna m0)
              m2      (zug-beenden m1)]

       (if (nil? (gewinner m2)) ;Wenn es keinen Gewinner gibt, kommt ein nächster Spielzug
         (when true
			       (update-raster m2 raster spieler-label) 
			       (sichern-nach "savegame.temp" m2)
			       (.setText schiffe-label (pprint-schiffe (get-in m2 [0 0]) (get m2 1)))
			       (clear-textpane vna)
			       (.repaint layerPane)
         )
         (when true ;Wenn es einen Gewinner gibt, dann wird dieser ausgegeben
           (.setText win-label (str "Spieler " (gewinner m2) " hat das Spiel gewonnen!"))
           (.removeAll layerPane)          
           (.add layerPane win-label)
           (.add layerPane wallpaper)
         )       
       )
)))))) 
;_____________________________________________________________________________________________
(defn init-schiffe-button 
    "Initialisierung des Action-Listeners für den Weitere-Schiffe-Button. Dieser ist abhängig von 
     sämtlichen Objekten, die dem Spielfenster zugeordnet sind."
    [ss layerPane raster schiffe-button vna spieler-label schiffe-label]
    (.setFont schiffe-button (Font. "SansSerif" (/ (. ss width) 90) (/ (. ss width) 90)))
		(. schiffe-button (addActionListener (proxy [ActionListener] [] (actionPerformed [evt]
         (let [m0      (laden-von "savegame.temp")
               m1      (check-senden vna m0)]
           (when true  
                (update-raster m1 raster spieler-label) 
                (.setText schiffe-label (pprint-schiffe (get-in m1 [0 0]) (get m1 1)))
                (sichern-nach "savegame.temp" m1)
                (clear-textpane vna)    
                (.repaint layerPane)
           )
         )          
     )))) 
)
;_____________________________________________________________________________________________
(defn init-spieler-label 
   "Bestimmt die Formatierung der Spieleranzeige.
    spieler-label: JLabel mit Namen des aktuellen Spielers
    screen-size: Dimension des Monitors für relative Positionsbestimmung des Labels
    m: Spielstandsinformation"
   [spieler-label screen-size m]
   (.setForeground spieler-label (. Color white))  
   (.setFont spieler-label (Font. "SansSerif" (/ (. screen-size width) 60) 
                                              (/ (. screen-size width) 60)))
   (.setBounds spieler-label (/ (. screen-size width) 20) 
                             (/ (. screen-size height) 20) 500 50)  
)
;_____________________________________________________________________________________________

(defn init-schiffe-label 
  "Initialisiert Anzeige für Schiffsbewegungen des aktuellen Spielers.
    schiffe-label: JLabel mit Bewegungen des aktuellen Spielers
    screen-size: Dimension des Monitors für relative Positionsbestimmung des Labels
    m: Spielstandsinformation"
  [schiffe-label screen-size m]
  (let [spieler (get-in m [0 0])]  
    (.setOpaque schiffe-label true)
    (.setBackground schiffe-label (Color. 0 0 0 128))
    (.setForeground schiffe-label (. Color white))
    (.setFont schiffe-label (Font. "SansSerif" (/ (. screen-size width) 80)
                                  (/ (. screen-size width) 80)))
    (.setBounds schiffe-label (/ (. screen-size width) 20) (/ (. screen-size height) 8) 
                              (/ (. screen-size width) 4) (/ (. screen-size height) 3)) 
    (.setBorder schiffe-label (LineBorder. (. Color white)))
    (.setText schiffe-label "Schiffsbewegungen:\n")
  )
)
;_____________________________________________________________________________________________

(defn init-win-label 
  "Initialisiert das JLabel für die Gewinneranzeige.
   win-label: Zeigt Namen der ermittelten Gewinners an.
   screen-size: Dimension des Monitors für relative Positionsbestimmung des Labels"
    [win-label screen-size]
    (.setOpaque win-label true)
    (.setBounds win-label 0 0 (. screen-size width) (. screen-size height))
    (.setBackground win-label (Color. 0 0 0 128))
    (.setForeground win-label (. Color white))
   (.setFont win-label (Font. "SansSerif" (/ (. screen-size width) 40) 
                                          (/ (. screen-size width) 40))) 
)
;_____________________________________________________________________________________________
(defn spiel-fenster 
  "Erzeugt ein JFrame als Spielefenster mit Feldraster und Bedienungselementen
   anhand einer Spielinformation 'm'."
  [m]
  (let [fenster             (javax.swing.JFrame. "Konquest")
        raster              (javax.swing.JPanel.)      
        layerPane           (javax.swing.JLayeredPane.)
        screen-size         (.getScreenSize (. Toolkit getDefaultToolkit))
        wallpaper           (JLabel. (ImageIcon.  "png\\space.jpg"))
        anzahl-felder       (* (get-in m [3 0])  (get-in m [3 1]))
        planet-pics         (get-planet-pics m screen-size)
        end-button          (JButton. "Spielzug Beenden")
        schiffe-button      (JButton. "Weitere Schiffe")
        von-nach-anzahl     (javax.swing.JPanel.)
        spieler-label       (JLabel.)
        schiffe-label       (JLabel. "" (. SwingConstants CENTER))
        win-label           (JLabel. "" (. SwingConstants CENTER))
        buttons             (JPanel.)]
        
    (.setLayout buttons (GridLayout. 1 2))
    (.setBounds buttons (/ (. screen-size width) 20) (/ (. screen-size height) 1.5) 
                        (/ (. screen-size width) 4) (/ (. screen-size height) 20))     
    (.setSize wallpaper 1920 1080)
    
    (init-schiffe-label schiffe-label screen-size m)
    (init-von-nach-anzahl  von-nach-anzahl screen-size)
    (init-spieler-label spieler-label screen-size m)    
    (init-end-button screen-size layerPane raster end-button von-nach-anzahl spieler-label schiffe-label win-label wallpaper)
    (init-schiffe-button screen-size layerPane raster schiffe-button von-nach-anzahl spieler-label schiffe-label)
    (init-raster m raster anzahl-felder screen-size planet-pics)
    (update-raster m raster spieler-label)
    (init-win-label win-label screen-size) 

    (.add buttons schiffe-button)
    (.add buttons end-button)
    (.add layerPane buttons)
    (.add layerPane schiffe-label)
    (.add layerPane spieler-label)
    (.add layerPane von-nach-anzahl)
    (.add layerPane raster)
    (.add layerPane wallpaper)   
    (.add fenster layerPane)
    (.setExtendedState fenster (. javax.swing.JFrame MAXIMIZED_BOTH))
    (.setLocationRelativeTo fenster nil)
    (.setVisible fenster true)  
  )
)
;_____________________________________________________________________________________________
(defn starte-spiel 
  "Erzeugt eine Spielfeldinformation anhand der Eingaben im (parameter-auswahl)-Fenster.
   groesse: String für die Dimensionalität des Spielfeldes, z.B. '6;6' -> 6 Zeilen und 6 Spalten 
   namen: String für die Namen der beteiligten Spieler
   anzahl-neutral: String für die Anzahl der zu berücksichtigen neutralen Planeten"
  [groesse namen anzahl-neutral]
   (let [m (erzeuge-spiel (into [] (for [x (str/split groesse #";")] (read-string x)))
                  (str/split namen #";") (read-string anzahl-neutral))]
       (sichern-nach "savegame.temp" m)
       (spiel-fenster m)
   )
)

;(starte-spiel "6;6" "Robin;Bially" "4")
;_____________________________________________________________________________________________
(defn parameter-auswahl 
  "Spieleinitialisierungsfenster für Eingabe der Spielernamen, 
   Felddimension und Anzahl neutraler Planeten"
  []
  (let [jframe (javax.swing.JFrame.)
        jlabel (JLabel. "Geben Sie die Spielernamen an:" (. SwingConstants CENTER))
        jlabel2 (JLabel. "Geben Sie die Größe des Spielfeldes an:" (. SwingConstants CENTER))
        jlabel3 (JLabel. "Geben Sie die Anzahl neutraler Planeten an:" (. SwingConstants CENTER))
        jtextarea (javax.swing.JTextArea. "Max;Ursula" )
        jtextarea2 (javax.swing.JTextArea. "6;6")
        jtextarea3 (javax.swing.JTextArea. "10")
        jpanel (javax.swing.JPanel. nil)
        starten (JButton. "Starten")
        laden (JButton. "Letzten Spielstand laden")]
	     
   (. starten (addActionListener (proxy [ActionListener] [] (actionPerformed [evt]
                    (when true
                     (starte-spiel 
                           (.getText jtextarea2) 
                           (.getText jtextarea) 
                           (.getText jtextarea3))
 
                     (.dispose jframe)
                   )
   ))))
   
   (. laden (addActionListener (proxy [ActionListener] [] (actionPerformed [evt]
                    (when true
                     (spiel-fenster (laden-von "savegame.temp"))
                     (.dispose jframe)
                   )
   ))))
 
     (.setLayout jpanel (GridLayout. 4 2 10 0))
     (.add (.getContentPane jframe) jpanel)
     
     (.setFont jtextarea (Font. "SansSerif" 22 22))
     (.setFont jtextarea2 (Font. "SansSerif" 22 22))
     (.setFont jtextarea3 (Font. "SansSerif" 22 22))    
     (.setFont jlabel (Font. "SansSerif" 22 22))
     (.setFont jlabel2 (Font. "SansSerif" 22 22))
     (.setFont jlabel3 (Font. "SansSerif" 22 22))
     (.setBorder jlabel (. LineBorder createBlackLineBorder))
     (.setBorder jlabel2 (. LineBorder createBlackLineBorder))
     (.setBorder jlabel3 (. LineBorder createBlackLineBorder))
          

	   (.setFont starten (Font. "SansSerif" 22 22))      
     (.setFont laden (Font. "SansSerif" 22 22))  

	   (.add jpanel jlabel) 
     (.add jpanel jtextarea)     
     (.add jpanel jlabel2)  
     (.add jpanel jtextarea2)
     (.add jpanel jlabel3)
     (.add jpanel jtextarea3)     
     (.add jpanel starten)
     (.add jpanel laden)
 
     (.setSize jframe 1000 200) 
     (.setLocationRelativeTo jframe nil)
     (.setVisible jframe true) 
  )
)

(parameter-auswahl)


