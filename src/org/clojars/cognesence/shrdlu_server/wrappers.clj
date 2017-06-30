
;this file contains high level glue & wrappers and their
;associalted functions...
;
;    uiwin -- the native Java window controls
;    comms -- the socket link to netlogo
;
;===========================================================
;
; load order...
;    matcher
;    ops-search
;    this file
;    strips-search
;    block-fns
;    nlp
;
;==========================================================



(require '[org.clojars.cognesence.matcher.core :refer :all])
(require '[clojure.set :refer :all])
(require '[clojure.string :as str])
(require '[clojure.pprint :refer :all])


;====================================
; window manufacture & control
;====================================

(import '(java.awt Font))

(declare
  set-atom!
  add-window
  )

;____ builders ______________________

(def uiwin (atom {}))

(defn setup-ui-windows []
  (set-atom! uiwin {})
  (let [f   (new javax.swing.JFrame)
        box (new javax.swing.Box javax.swing.BoxLayout/Y_AXIS)
        ]
    (.setSize f 500 500)
    (.setVisible f true)
    (.add f box)

    (add-window box :dbg  "reasoning details")
    (add-window box :comm "communication")
    (add-window box :bdat "belief system of planner")
    ))


(defn add-window
  ([box name label] (add-window box name label 7 50))
  ([box name label row col]
    (let [ta (new java.awt.TextArea row col)]
      (.setFont ta (new Font Font/MONOSPACED Font/PLAIN 12))
      (.add box (new javax.swing.JLabel label))
      (.add box ta)
      ;; if you want cognesence colors
;      (.setBackground ta (new java.awt.Color 13 58 79))
;      (.setForeground ta java.awt.Color/WHITE)
      ;; finally
      (swap! uiwin assoc name ta)
      )))


;____ window dispatch ___________________

(def win-mode true)

;(defn ui-out
;  [& r]
;  (if-not win-mode (apply println r)
;    (let [[win & r] r]
;      (.append (win @uiwin) (write r :stream nil))
;      (.append (win @uiwin) "\n")
;      )))

(defn ui-out
  [win & r]
  (if-not win-mode (apply println r)
   ;; (let [[win & r] r]
    (do
      (doseq [x r]
        (cond
          (nil? x)
          (.append (win @uiwin) "nil")

          (coll? x)
          (.append (win @uiwin) (write x :stream nil))

          (symbol? x)
          (.append (win @uiwin) (str x))

          :else
          (.append (win @uiwin) x)
          )
        (.append (win @uiwin) " "))
      (.append (win @uiwin) "\n")
      )))


(defn ui-broadcast [stuff]
  (if-not win-mode (apply println stuff)
    (doseq [io (keys @uiwin)]
      (ui-out io stuff))
    ))



(defn ui-set
  [& r]
  (if-not win-mode (apply println r)
    (let [[win & r] r]
      (.setText (win @uiwin) (write r :stream nil))
      )))


(defn clear-ui []
  (if win-mode
    (doseq [io (keys @uiwin)]
      (.setText (io @uiwin) ""))
    ))




;====================================
; socket manufacture & control
;====================================


(import '(java.net ServerSocket Socket SocketException)
  '(java.io InputStreamReader OutputStreamWriter)
  '(clojure.lang LineNumberingPushbackReader))


;___ active socket is used as a global _____________

(def shrdlu-comms false)

(defn startup-server
  [port]
  (let [ss (new ServerSocket port)]
    (println "advertising " ss)
    (try (let [s (.accept ss)]
           (println "socket accepted " s)

           { :sock s
             :inp  (new LineNumberingPushbackReader
                     (new InputStreamReader (.getInputStream s)))
             :outp (new OutputStreamWriter (.getOutputStream s))
             }
           )
      (catch SocketException e))
    ))


(defn socket-write
  "low-level socket writer"
  [socket x]
  (binding [*out* (:outp socket)]
    (println x)
    ))


(defn socket-read
  "low-level socket reader"
  [socket]
  (read (:inp socket)))

(defn socket-input-waiting
  [socket]
  (.ready (:inp socket)))


;___ netlogo reading/writing _____________

(defn set-shrdlu-comms [port]
  (def shrdlu-comms (startup-server port)))

(defn nlogo-send [txt]
  ;(println '** (and shrdlu-comms true) txt)
  (if shrdlu-comms (socket-write shrdlu-comms txt)))

(defn nlogo-read []
  (if shrdlu-comms (socket-read shrdlu-comms)))

(defn nlogo-io-waiting []
  (and shrdlu-comms (socket-input-waiting shrdlu-comms)))


(declare nlogo-translate-cmd)

(defn nlogo-send-exec [cmd-list]
  ; (ui-out :comm 'NL==> cmd-list)
  (let [cmd-str (nlogo-translate-cmd cmd-list)]
    (ui-out :comm 'NL==> cmd-list "   \t" cmd-str)
    ; (ui-out :comm "     " cmd-str)
    (nlogo-send cmd-str)
    ))

;user=> (def s25 (startup-server 2222))
;advertising  #<ServerSocket ServerSocket[addr=0.0.0.0/0.0.0.0,port=0,localport=2225]>
;socket accepted  #<Socket Socket[addr=/152.105.17.36,port=55053,localport=2225]>
;#'user/s25
;user=> s25
;{:sock #<Socket Socket[addr=/152.105.17.36,port=55053,localport=2225]>, :inp #<LineNumberingPushbackReader clojure.lang.LineNumberingPushbackReader@176feac>, :outp #<OutputStreamWriter java.io.OutputStreamWriter@dc033a>}
;user=> (socket-read s25)
;test-message-1
;user=> (socket-read s25)
;test-message-2
;user=> (socket-write s25 "banana")
;nil


