(require '[org.clojars.cognesence.opsearch.core :refer :all])

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



(defn set-atom! [atm x]
  (swap! atm (fn [_] x)))


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

