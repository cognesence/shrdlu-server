(ns org.clojars.cognesence.shrdlu-server.core
  ^{:doc "The server side of the SHRDLU example used at ELS 2015 and Game On."
    :author "Simon Lynch"}
  (:require [clojure.set :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all])
  (:gen-class))
  
;====================================
; core
;====================================

(defn set-atom! [atm x]
  (swap! atm (fn [_] x)))
  
;====================================
; window manufacture & control
;====================================
  
(import '(java.awt Font TextArea)
        '(javax.swing Box BoxLayout JFrame JLabel))

(def uiwin (atom {}))

(defn add-window
  ""
  ([box name label] (add-window box name label 7 50))
  ([box name label row col]
    (let [ta (new TextArea row col)]
      (.setFont ta (new Font Font/MONOSPACED Font/PLAIN 12))
      (.add box (new JLabel label))
      (.add box ta)
      (swap! uiwin assoc name ta))))

(defn setup-ui-windows 
  ""
  []
  (set-atom! uiwin {})
  (let [f (new JFrame)
        box (new Box BoxLayout/Y_AXIS)]
    (.setSize f 500 500)
    (.setVisible f true)
    (.add f box)
    (add-window box :dbg  "reasoning details")
    (add-window box :comm "communication")
    (add-window box :bdat "belief system of planner")))

(def win-mode true)

(defn ui-out
  ""
  [win & r]
  (if-not win-mode (apply println r)
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
          (.append (win @uiwin) x))
        (.append (win @uiwin) " "))
      (.append (win @uiwin) "\n"))))

(defn ui-broadcast [stuff]
  ""
  (if-not win-mode (apply println stuff)
    (doseq [io (keys @uiwin)]
      (ui-out io stuff))))

(defn ui-set
  ""
  [& r]
  (if-not win-mode (apply println r)
    (let [[win & r] r]
      (.setText (win @uiwin) (write r :stream nil)))))

(defn clear-ui 
  ""
  []
  (if win-mode
    (doseq [io (keys @uiwin)]
      (.setText (io @uiwin) ""))))

;====================================
; socket manufacture & control
;====================================

(import '(java.net ServerSocket Socket SocketException)
        '(java.io InputStreamReader OutputStreamWriter)
        '(clojure.lang LineNumberingPushbackReader))
 
(def shrdlu-comms false)

(defn startup-server
  ""
  [port]
  (let [ss (new ServerSocket port)]
    (println "advertising " ss)
    (try (let [s (.accept ss)]
           (println "socket accepted " s)
           {:sock s
            :inp (new LineNumberingPushbackReader
                  (new InputStreamReader (.getInputStream s)))
            :outp (new OutputStreamWriter (.getOutputStream s))})
      (catch SocketException e))))

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

;=======================================================================================
;=======================================================================================
;=======================================================================================
;=======================================================================================

(defn print-goals [q]
  (if (not (empty? q))
    (do
      (ui-out :dbg "GOALS:")
      (doseq [x q]
        (ui-out :dbg "      " (if (map? x) [(:name x) :=> (:achieves x)] x))
        )
      ;(ui-out :dbg '------)
      )
    ))


(def goalq (atom (java.util.concurrent.LinkedBlockingDeque.)))


(declare strips-loop update-path
  goal-mop-apply apply-goal-op)

(defn strips-solver [state goal goal-ops]
  (.clear @goalq)
  (.push @goalq goal)
  (strips-loop {:state state, :cmds nil, :txt nil} goal-ops 60))


(defn strips-loop
  [path goal-ops limit]
  (if (zero? limit)
    (throw (new RuntimeException "limit exceeded in run-goal-ops")))

  ;(println path)
  (print-goals @goalq)

  (if-let [goal (.poll @goalq)]
    (cond
      (map? goal) ;; it is a partially matched op
      (do
        (ui-out :dbg '** 'APPLYING (:name goal) '=> (:achieves goal))
       ; (ui-out :dbg '** (:add goal))
        (recur
          (update-path path (goal-mop-apply (:state path) goal))
          goal-ops (dec limit))
        )

      ;; else it is a fact
      (not (contains? (:state path) goal))
      (do (ui-out :dbg 'solving goal)
        (some (partial apply-goal-op (:state path) goal)
              (vals goal-ops))
        (recur path goal-ops (dec limit))
        )
      ;; else it is an existing fact
      :else
      (recur path goal-ops (dec limit))
      )
    path
    )
  )


(defn goal-mop-apply [bd mop]
  (mfind* [(:pre mop) bd]
    (ui-out :dbg '** (mout (:add mop)))
   ; (ui-out :dbg '=> (mout mop))
    {:state (union (mout (:add mop))
              (difference bd (mout (:del mop))))
     :cmd   (mout (:cmd mop))
     :txt   (mout (:txt mop))
     }
    ))


(defn apply-goal-op [bd goal op]
  ;(println (list 'trying (:name op)))
  (mlet [(:achieves op) goal]

    (mfind* [(:when op) bd]
      (ui-out :dbg 'using=> (:name op))
      (let [mop (mout op)]
        ;(println (list 'new-mop mop))
        (.push @goalq mop)
        (ui-out :dbg 'new-goals (or (:post mop) '-none))
        (doseq [p (reverse (:post mop))]
          (.push @goalq p))

        ;(println (list 'succeeded (:name op)))
        true
        ))
    ))


(defn update-path
  [current newp]
  { :state (:state newp),
    :cmds  (concat (:cmds current) (:cmd newp)),
    :txt   (concat (:txt current) (:txt newp))
    })

;=======================================================================================
;=======================================================================================
;=======================================================================================
;=======================================================================================

;======================================
; general settings
;======================================

(def settings {:search-type :strips})

;======================================
; morphology rules
;======================================

; std rules

(def sentence-morph-rules
  ; NB: these rules work on a single pass basis so
  ;     you MUST prioritize rules in the order you
  ;     want them applied

  '( ((??a and then ??b)  => (??a stop ??b))
     ((??a and ??b)       => (??a stop ??b))
     ((??a then ??b)      => (??a stop ??b))
     ((??a now ??b)       => (??a stop ??b))
     ))


(def word-match-rules
  ; NB: these rules work on a single pass basis so
  ;     you MUST prioritize rules in the order you
  ;     want them applied

  '( ((-> ?s is-size?)  size    => ?s)
     ((-> ?s is-size?)  sized   => ?s)
     ((-> ?c is-color?) color   => ?c)
     ((-> ?c is-color?) colour  => ?c)
     ((-> ?c is-color?) colored   => ?c)
     ((-> ?c is-color?) coloured  => ?c)

     (onto             => on)
     (on to            => on)
     (on to the top of => on)


     (on top of        => on)
     (on the top of    => on)

     (pick up          => grasp)
     (pickup           => grasp)
     (make a new       => make a)
     ))



;====================================
; standard search planning operators
;====================================


(def block-ops
  '{ pickup
     { :pre ( (hand empty)
              (cleartop ?x)
              (isa ?x ?_)
              (on  ?x ?y)
              (at  ?x ?s)
              )
       :del ( (hand empty)
              (cleartop ?x)
              (on  ?x ?y)
              (at  ?x ?s)
              )
       :add ( (holds ?x)
              (cleartop ?y)
              )
       :txt (pick ?x off ?y at ?s)
       :cmd (pick-from ?s)
       }
     drop
     { :pre ( (holds ?x)
              (cleartop ?y)
              (at ?y ?s)
              )
       :del ( (holds ?x)
              (cleartop ?y)
              )
       :add ( (hand empty)
              (cleartop ?x)
              (on  ?x ?y)
              (at  ?x ?s)
              )
       :txt (drop ?x on ?y at ?s)
       :cmd (drop-at ?s)
       }
     })



;====================================
; goal focussed operators
;====================================


(def goal-ops
  '{ :move-x1   ;; a handy multi-move operator when x & y on same s
;     { :name move-x1
;       :achieves (on ?x ?y)
;       :when ((isa ?x ?_) (at ?x ?s) (at ?y ?s))
;       :post ((protected ?s [on ?x ?y]) (holds ?x) (on ?x ?y))
;       :del ((protected ?s [on ?x ?y]) )
;       }
     { :name move-x1
       :achieves (on ?x ?y)
       :when ((isa ?x ?_) (at ?x ?s) (at ?y ?s))
       :post ((cleartop ?x) (on ?x ?y))
       }

     :move-x   ;; a handy multi-move operator
     { :name move-x
       :achieves (on ?x ?y)
       :when ((isa ?x ?_) (at ?x ?sx) (at ?y ?sy) )
       :post ((protected ?sx [on ?x ?y]) (protected ?sy [on ?x ?y])
               (cleartop ?x) (cleartop ?y) (hand empty) )
       :pre ((on ?x ?ox) )
       :del ((at ?x ?sx)  (on ?x ?ox) (cleartop ?y)
             (protected ?sx [on ?x ?y]) (protected ?sy [on ?x ?y]) )
       :add ((at ?x ?sy) (on ?x ?y) (cleartop ?ox))
       :cmd ((pick-from ?sx) (drop-at ?sy) )
       :txt ((mv-pick ?x off ?ox at ?sx)
             (mv-put ?x on ?y at ?sy) )
       }

     :cleartop
     { :name cleartop
       :achieves (cleartop ?x)
       :when ((on ?z ?x) (at ?z ?s))
       :post ((protected ?s [cleartop ?x]) (cleartop ?z) (hand empty) )
       :pre  ((stack ?new)  (:not (protected ?new ?_))
              (at ?y ?new)  (cleartop ?y) )
       :del ((protected ?s [cleartop ?x])
             (on ?z ?x) (at ?z ?s) (cleartop ?y) )
       :add ((cleartop ?x) (at ?z ?new) (on ?z ?y)    )
       :cmd ((pick-from ?s) (drop-at ?new) )
       :txt ((ct-pick ?z off ?x at ?s)
             (ct-put ?z on ?y at ?new) )
       }

     :drop
     { :name drop
       :achieves (hand empty)
       :when ((holds ?x))
       :pre  ((stack ?s) (:not (protected ?s ?_))
              (at ?y ?s) (cleartop ?y)  )
       :del  ((holds ?x) (cleartop ?y) )
       :add  ((at ?x ?s) (on ?x ?y) (hand empty) (cleartop ?x))
       :cmd  ((drop-at ?s) )
       :txt  ((put ?x on ?y at ?s) )
       }

     :pickup-x
     { :name pickup-x
       :achieves (holds ?x)
       :when ((isa ?x ?_) (at ?x ?s) (on ?x ?y))
       :post ((cleartop ?x) (hand empty))
       :del  ((at ?x ?s) (on ?x ?y) (hand empty) (cleartop ?x))
       :add  ((holds ?x) (cleartop ?y) )
       :cmd  ((pick-from ?s))
       :txt  ((pick ?x off ?y at ?s))
       }

     :drop-at
     { :name drop-at
       :achieves (at ?x ?s)
       :post ((holds ?x))
       :pre  ((at ?y ?s)
              (cleartop ?y)  )
       :del  ((holds ?x) (cleartop ?y) )
       :add  ((at ?x ?s) (on ?x ?y) (hand empty) (cleartop ?x))
       :cmd  ((drop-at ?s) )
       :txt  ((put ?x on ?y at ?s) )
       }

     :drop-on
     { :name drop-on
       :achieves (on ?x ?y)
       :when ((isa ?x ?_) (at ?y ?s))
       :post ((protected ?s [on ?x ?y])(cleartop ?y)(holds ?x))
       :pre  ()
       :del  ((holds ?x) (cleartop ?y) (protected ?s [on ?x ?y]))
       :add  ((at ?x ?s) (on ?x ?y) (hand empty) (cleartop ?x))
       :cmd  ((drop-at ?s) )
       :txt  ((put ?x on ?y at ?s) )
       }

     :drop-on-held
     { :name drop-on-held
       :achieves (on ?x ?y)
       :when ((holds ?y))
       :post ((hand empty) (on ?x ?y))
       }

     :protect-x
     { :name protect-x
       :achieves (protected ?x ?c)
       :add  ((protected ?x ?c)  )
       }
     })



;====================================
; operators for executive calls
;====================================


(def exec-ops
  '{ grasp
     { :pre ( (hand empty)
              (at ?x ?s)
              (on ?x ?y)
              (cleartop ?x)
              (isa ?x ?_)
              )
       :del ( (hand empty)
              (at ?x ?s)
              (on ?x ?y)
              )
       :add ( (holds ?x)
              (cleartop ?y)
              )
       :txt (grasp! ?x off ?y at ?s)
       :cmd (pick-from ?s)
       }
     puton
     { :pre ( (holds ?x)
              (at ?dst ?s)
              (cleartop ?dst)
              )
       :del ( (holds ?x)
              (cleartop ?dst)
              )
       :add ( (hand empty)
              (on ?x ?dst)
              (at ?x ?s)
              (cleartop ?x)
              )
       :txt (put! ?x on ?dst at ?s)
       :cmd (drop-at ?s)
       }
     create
     { :pre ( (hand empty) )
       :del ( (hand empty) )
       :add ( (isa  ?x ?isa)
              (size ?x ?size)
              (color ?x ?color)
              (holds ?x)
              )
       :txt (make! ?x ?isa size= ?size color= ?color)
       :cmd (make ?x ?isa ?size ?color)
       }
     dispose
     { :pre ( (holds ?x)        (isa ?x ?obj)
              (color ?x ?color) (size ?x ?size)
              )
       :del ( (holds ?x) (cleartop ?x) (isa ?x ?obj)
              (color ?x ?color) (size ?x ?size)
              )
       :add ( (hand empty) )
       :txt (dispose! ?x)
       :cmd (dispose)
       }})



;====================================================
; mapping for resolved parses to search goals, etc
;====================================================

(declare
  goal apply-exec
  extract-to-map clear-block-data
  it-reference)

(def default-block-spec '{color grey, size med, shape cube})


(defmatch process-cmd []
  ((grasp ?x)        :=> (set-atom! it-reference (? x))   (goal (mout '(holds ?x))))
  ((put-on ?x ?y)    :=> (set-atom! it-reference (? x))   (goal (mout '(on ?x ?y))))
  ((put-at ?x ?s)    :=> (set-atom! it-reference (? x))   (goal (mout '(at ?x ?s))))
  ((move-hand-to ?s) :=> (apply-exec ('puton exec-ops) (mout '{s ?s})))
  ((create ?x ?spec) :=>
    (goal '(hand empty))
    (set-atom! it-reference (? x))
    (apply-exec ('create exec-ops)
      (conj {'x (? x)}
        (merge default-block-spec
          (extract-to-map (? spec))))))
  ((destroy ?x)      :=>
    (goal (mout '(holds ?x)))
    (apply-exec ('dispose exec-ops))
    (set-atom! it-reference false))
  ((reset)         :=> (clear-block-data) (nlogo-send "setup"))
  ; ( ?x             :=> (ui-out :dbg 'ERROR '(unknown NetLogo request) (? x)))
  )



;================================
; Netlogo comms & filters
;================================


(let [sizes '{small 5, med 7, large 9}
      sp " "
      qt (char 34)
      str-qt (fn[x] (str qt x qt sp)) ; Wrap `x` in quotes.
      stack-no (fn[x] (apply str (rest (str x))))] ; strip `s` of stack name
  (defmatch nlogo-translate-cmd []
    ((make ?nam ?obj ?size ?color) :=> (str 'exec.make (str-qt (? nam)) (str-qt (? obj)) ((? size) sizes) (str-qt (? color))))
    ((move-to ?s) :=> (str 'exec.move-to sp (stack-no (? s))))
    ((drop-at ?s) :=> (str 'exec.drop-at sp (stack-no (? s))))
    ((pick-from ?s) :=> (str 'exec.pick-from sp (stack-no (? s))))
    (?_ :=> (ui-out :dbg 'ERROR '(unknown NetLogo cmd)))))
    
;=======================================================================================
;=======================================================================================
;=======================================================================================
;=======================================================================================

; forward declarations

(declare
  word-check pnp-sems edit

  block-data bd-set! bd-del! bd-add!

  gen-block-name
  nlogo-send-exec
  )


;==========================================
; globals
;==========================================

(def it-reference (atom false))



;==========================================
; lexicon
;==========================================

(def lexicon
  '{blue   {:cat adj, :sem (color ?x blue)}
    red    {:cat adj, :sem (color ?x red)}
    green  {:cat adj, :sem (color ?x green)}
    grey   {:cat adj, :sem (color ?x grey)}
    orange {:cat adj, :sem (color ?x orange)}
    brown  {:cat adj, :sem (color ?x brown)}
    yellow {:cat adj, :sem (color ?x yellow)}
    pink   {:cat adj, :sem (color ?x pink)}

    large {:cat adj, :sem (size ?x large)}
    small {:cat adj, :sem (size ?x small)}

    block {:cat noun, :sem (isa ?x cube)}
    box   {:cat noun, :sem (isa ?x cube)}
    brick {:cat noun, :sem (isa ?x cube)}
    cube  {:cat noun, :sem (isa ?x cube)}

    sphere {:cat noun, :sem (isa ?x sphere)}
    circle {:cat noun, :sem (isa ?x sphere)}
    ball   {:cat noun, :sem (isa ?x sphere)}

    thing  {:cat noun, :sem (isa ?x ?_)}

    pyramid   {:cat noun, :sem (isa ?x pyramid)}
    triangle  {:cat noun, :sem (isa ?x pyramid)}

    the    {:cat det} ;, :sem undef}
    a      {:cat det} ;, :sem undef}
    an     {:cat det}
    any    {:cat det}

    on      {:cat prep, :sem (on ?y ?x)}
    under   {:cat prep, :sem (on ?x ?y)}

    grasp   {:cat verb1, :arity 1, :sem grasp}
    find    {:cat verb1, :arity 1, :sem grasp}

    remove  {:cat verb1, :arity 1, :sem destroy}
    destroy {:cat verb1, :arity 1, :sem destroy}

    make    {:cat make, :arity 1, :sem make}
    create  {:cat make, :arity 1, :sem make}

    place   {:cat put2, :arity 2, :sem put-on}
    move    {:cat put2, :arity 2, :sem put-on}
    put     {:cat put2, :arity 2, :sem put-on}
    })





;==========================================
; grammar forms
;==========================================



;___ word forms ___________________________

(defn adj?   [x] (word-check 'adj x))
(defn det?   [x] (word-check 'det x))
(defn noun?  [x] (word-check 'noun x))
(defn prep?  [x] (word-check 'prep x))
(defn verb1? [x] (word-check 'verb1 x))  ;; verb with arity 1
(defn put2?  [x] (word-check 'put2 x))   ;; verb with arity 1
(defn make?  [x] (word-check 'make x))

;___ world context predicates ______________

(defn stack? [x] (mfind [`(~'stack ~x) @block-data] true))
(defn block? [x] (mfind [`(~'isa ~x ~'?_) @block-data] true))

(defn id-type? [x]    (and (map? x) (= (:type x) 'id)))
(defn tuple-type? [x] (and (map? x) (= (:type x) 'tuples)))



;___ noun group ___________________________

(defmatch noun-group []
  (((-> ?d det?) (-> ?n noun?))       ; NG -> Det N
    :=> {:cat  'ng
         :id   '?x,    ; by default
         :type 'tuples
         :sem  (list (? n))
         }
    )
  (((-> ?d det?) (-> ??a adjG) (-> ?n noun?))    ; NG -> Det AdjG N
    :=> {:cat  'ng
         :id   '?x,    ; by default
         :type 'tuples
         :sem  (mout '(??a ?n))
         }
    ))



;___ adj group ___________________________

(defn adjG [lis]                   ; AdjG -> *Adj
  (and (every? #(adj? %) lis)
    (map #(adj? %) lis)
    ))


;___ noun phrase ___________________________

(defmatch noun-phrase []
  (((-> ??np noun-group)) :=> (assoc (? np) :cat 'np))   ; NP -> NG
  (((-> ?obj block?))                                    ; NP -> block
    :=> {:cat 'np, :type 'id :sem  (? obj)}
    )
  (((-> ?obj #(= % 'it)))                 ; NP -> block
    :=> {:cat 'np, :type 'id :sem @it-reference}
    )
  (((-> ??np noun-group) (-> ??pp prep-phrase))          ; NP -> NG PP
    :=> (let [sym (gensym 'x)]
          {:id   '?x
           :cat  'np
           :type 'tuples
           :sem  (concat (:sem (? np)) (:sem (? pp)))
           }))
  )



;___ prep phrase ___________________________

(defmatch prep-phrase []
  (((-> ?prep prep?) (-> ??np noun-phrase))             ; PP -> Prep NP
    :=> (pnp-sems (? prep) (? np))
    ))



;___ cmd phrase ___________________________
; this is the top level phrase

(defmatch parse []
  (((-> ?cmd verb1?) (-> ??obj noun-phrase))
    :=> (list (? cmd) (? obj))
    )
  (((-> ?cmd make?) (-> ??obj noun-group))
    :=> (let [obj (? obj)
              id  (gen-block-name)
              ]
          (list 'create id
            (edit (:id obj) id (:sem obj))))
    )
  (((-> ?cmd put2?) (-> ??obj noun-phrase) on (-> ?s stack?))
    :=> (list 'put-at (? obj) (? s))
    )
  (((-> ?cmd put2?) (-> ??obj1 noun-phrase) on (-> ??obj2 noun-phrase))
    :=> (list (? cmd) (? obj1) (? obj2))
    )
  )





;======================================
; morphology
;======================================



(defn compile-word-rules
  "extends rules so they may be more easily applied"
  ; (?x ?y -> ?z) becomes (??a ?x ?y ??b -> ??a ?z ??b)
  [rules]
  (with-mvars {'aa (gensym '??a), 'bb (gensym '??b)}
    (mfor ['(??pre => ??post) rules]
      (mout '((?aa ??pre ?bb) => (?aa ??post ?bb)))
      )))


(defn apply-morph-rules [rules sentence]
  (if (empty? rules) sentence
    (mlet ['(?pre => ?post) (first rules)]
      (mif [(? pre) sentence]
        (recur rules (mout (? post)))
        (recur (rest rules) sentence)
        ))
    ))


(defn is-size? [x]
  (if-let [m (x lexicon)]
    (and (matches '(size ?_ ?_) (:sem m))
      true)
    ))


(defn is-color? [x]
  (if-let [m (x lexicon)]
    (and (matches '(color ?_ ?_) (:sem m))
      true)
    ))


(let [morph-rules (concat (compile-word-rules word-match-rules)
                    sentence-morph-rules)]
  (defn morph [sentence]
    (apply-morph-rules morph-rules sentence))
  )




;==========================================
; utilities
;==========================================


;___grammar semantic utils_________________

(defn edit [old new tree]
  (cond
    (and (seq? tree) (empty? tree)) tree

    (= tree old)  new

    (seq? tree)   (cons (edit old new (first tree))
                    (edit old new (rest tree)))
    :else
    tree
    ))


(defn pnp-sems [p np]
  ;(println 'p= p 'np= np)
  (cond
    (id-type? np)                          ; NP is a block name
    (let [sem (edit '?y '?x                ; promote ?y (naieve semantics!!)
                (edit '?x (:sem np) p))    ; name substitution
          ]
      {:id '?x, :cat 'pp, :sem sem}
      )

    (tuple-type? np)                   ; NP is a set of tuples
    (let [sym (gensym '?x)
          sem (edit '?y '?x      ; promote ?y (naieve semantics!!)
                (edit '?x sym    ; standard substitution
                  (cons p (:sem np)) ))
          ]
      {:id '?x, :cat 'pp, :sem sem}
      )

    :else
    (throw (Exception. "pnp-sems: unknown type"))
    ))


;___grammar syntactic utils________________

(declare find-obj)

(defn word-check [wtype word]
  (if-let [wdef (word lexicon)]
    (if (= (:cat wdef) wtype)
      (or (:sem wdef) 'undef)
      )))



(defn find-obj [spec]
  (ui-out :comm 'finding spec)
  (if (= (:type spec) 'id)
    (do (ui-out :comm 'found (:sem spec))
      (:sem spec))
    (let [vnam (symbol (subs (str (:id spec)) 1))  ;; strip-off "?"
          obj (mfind* [(:sem spec) @block-data] (get mvars vnam))
          ]
      (ui-out :comm 'found obj)
      (if (nil? obj) (throw (Exception. (str "whoops- I cannot find a " spec))))
    obj
    )))

(defn resolve-objs [spec]
  (map #(if (map? %) (find-obj %) %) spec))

(defn check [tuples text]
  (or (mfind* [tuples @block-data] true)
    (do (ui-out :comm 'mishap text)
      false
      )))



;=============================================
; utility fns for mapping exec cmds
;=============================================


(defn extract-to-map
  [spec]
  (reduce conj (map (fn [[r _ v]] {r v}) spec)))


(defn apply-exec
  "like apply-op but uses selected matcher bindings & implicitly
    applies to @block-data"
  ([op] (apply-exec op {}))
  ([op bind]
    (with-mvars bind
      (mfind* [(:pre op) @block-data]
        (let [op (mout op)]
          (bd-del! (:del op))
          (bd-add! (:add op))
          (nlogo-send-exec (:cmd op))
          (:txt op)
          )))
    ))



(defn goal [g]
  (ui-out :dbg 'goal g)
  (let [smap
        (cond
          (= (:search-type settings) :breadth-first)
          (ops-search @block-data (list g) block-ops)

          (= (:search-type settings) :strips)
          (strips-solver @block-data g goal-ops)

          :else
          (throw (new RuntimeException "unknown search type in settings"))
          )]

  (if-not smap
    ;; search failed
    (do (ui-out :dbg "Help! - I cannot find a way to do this")
      nil
      )
    ;; otherwise
    (do
      (ui-out :dbg 'solved...)
      ; (ui-out :dbg smap)
      (ui-out :dbg 'plan= (:txt smap))
      (ui-out :dbg 'cmds= (:cmds smap))
      (doseq [c (:cmds smap)]
        (nlogo-send-exec c))
      (bd-set! (:state smap))
      (ui-set :bdat @block-data)
      )
    )))


;(defn goal [g]
;  (ui-out :dbg 'goal g)
;  (if-let [smap (ops-search @block-data (list g) block-ops)]
;    (do
;      (ui-out :dbg 'solved...)
;      ; (ui-out :dbg smap)
;      (ui-out :dbg 'plan= (:txt smap))
;      (ui-out :dbg 'cmds= (:cmds smap))
;      (doseq [c (:cmds smap)]
;        (nlogo-send-exec c))
;      (bd-set! (:state smap))
;      (ui-set :bdat @block-data)
;      )
;    ; else - search failed
;    (do (ui-out :dbg "Help! - I cannot find a way to do this")
;      nil
;      )
;    ))



;================================
; general utils
;================================


(defn TODO! [& r]
  (println "**__ TODO __________")
  (apply println "** " r)
  (println "**__________________")
  )


;================================
; block-data
;================================

(def no-stacks 8)

(defn gen-stack-names [n]
  (map #(symbol (str/join (list "s" (str %))))
    (range n)))



(defn gen-stack-tuples [s-names]
  (into #{}
    (reduce concat
      (for [s s-names]
        (with-mvars {'s s}
          (mout '((stack ?s) (cleartop ?s) (at ?s ?s)))
          )))
    ))



(let [n (atom 0)]
  (defn gen-block-name []
    (symbol (str/join (list "b" (str (swap! n inc)))))
    )
  (defn reset-block-numbering [] (swap! n (fn [_] 0)))
  )


(def block-data (atom #{}))

(defn bd-set! [data]
  (set-atom! block-data data))

(defn bd-add! [tuples]
  (set-atom! block-data (union @block-data (set tuples)))
  )

(defn bd-del! [tuples]
  (set-atom! block-data (difference @block-data (set tuples))))


(defn clear-block-data []
  (reset-block-numbering)
  (ui-out :dbg 'resetting 'block-data)
  (set-atom! block-data #{})
  (bd-add! (gen-stack-tuples (gen-stack-names no-stacks)))
  (bd-add! '#{(hand empty)})
  (ui-set :bdat @block-data)
  @block-data
  )


(defn get-held []
  (mfind ['(holds ?x) @block-data] (? x)))


(defn pbd [] (pprint @block-data))


;================================
; top level repl's
;================================

(declare sentence- shrep-1)

(defmatch sentence []
  ((??s stop ??rest) :=> (sentence- (? s))
                          (sentence (? rest)))
  ( ?s  :=>  (sentence- (? s)))
  )

(defn sentence- [sentence]
  (ui-out :comm "I get:   " sentence)
  (if-let [ptree (parse sentence)]
    (do
      (ui-out :comm 'nlp ptree)
      (let [cmd (resolve-objs ptree)]
        (ui-out :comm 'cmd cmd)
        (ui-out :dbg 'processing...)
        (process-cmd cmd)
        (ui-set :bdat @block-data))
      ;true
      )
    ;else
    (ui-out :comm "MISHAP: I do not understand")
    )
  true
  )


(defn shrepl []
  (loop []
    (let [in-str  (read-line)
          in-list (map symbol (str/split in-str (re-pattern #" ")))
          ]
      (if (= in-list '(exit))
        (do (nlogo-send "stop")
          'shrepl-exits)
        (do (shrep-1 in-list)
          (recur))
      ))))


(defn shrep-1 [in-list]
  (ui-broadcast "_________________\n")
  (ui-out :comm "I heard: " in-list)
  (sentence (morph in-list))
  )

;(defn shrep-1 [in-list]
;  (ui-broadcast "_________________\n")
;  (ui-out :comm "I heard: " in-list)
;  (if (= (first in-list) 'exec)
;    (exec (rest in-list))
;    (sentence (morph in-list))
;    ))


(defn shrep-data [lists]
  (doseq [x lists] (shrep-1 x)))


(defn reset []
  (clear-ui)
  (clear-block-data)
  ;(shrepl)
  )


(defn exit []
  (nlogo-send "stop"))


(defn startup [port]
  (setup-ui-windows)
  (set-shrdlu-comms port)
  (reset)
  )


(def sample-test
  '(make a blue box and put it on s3
     then make a red block and put it on the blue box
     then make a new yellow box
     then put it on s2
     then make an orange triangle and put it on b1
     then make a brown ball and put it on b3
     then put b2 on the ball
     then pick up b4
     then put b1 on b4
     then put b3 on s1
     then put b4 on b3
     then make a box and put it on b1
     then make a small pink box and put it on the grey box
     ))


(def els-test
  '(make a blue box and put it on s3
     then make a red block and put it on the blue box
     then make a new yellow box
     then put it on s2
     then make an orange triangle and put it on s1
     then make a brown ball and put it on b3
     then put b2 on the ball
     then make a box and put it on b1
     then make a small pink box and put it on the grey box
     ))

(def video-prep
  '(make a blue box and put it on s3
     then make a red block and put it on the blue box
     then make a new yellow box
     then put it on s2
     then make an orange triangle and put it on s1
     then make a brown ball and put it on b3
     then put b2 on the ball
     then make a box and put it on b1
     then make a small pink box and put it on the grey box
    then put b5 on s4 then put b4 on b5
    then make a small green box and put it on b3
    ;then do...
    ; put b3 on the blue box then pick up the small pink box and put it on b3
     ))

;(shrep-1 els-test)
;(shrep-1 '(now put b2 on b1 and put b6 on b2))



(defn run-repeatedly []
  (for [x (range 1 5)]
    (do
      (shrep-1 '(put b7 on s4 then put b3 on b7))
      (shrep-1 '(put b7 on s2 then put b2 on b7))
      )))
      
;=======================================================================================
;=======================================================================================
;=======================================================================================
;=======================================================================================


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (startup 3333))
