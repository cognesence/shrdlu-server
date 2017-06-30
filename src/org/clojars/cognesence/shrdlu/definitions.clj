(require '[org.clojars.cognesence.matcher.core :refer :all])

;======================================
; general settings
;======================================

(def settings
  {
   ;; :search-type :breadth-first
    :search-type :strips
   })


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
      sp    " "
      qt    "\""
      str-qt   (fn[x] (str " \"" x "\" "))    ; wrap x in quotes
      stack-no (fn[x] (apply str (rest (str x))))   ; strip "s" of stack name
      ]


  (defmatch nlogo-translate-cmd []
    ((make ?nam ?obj ?size ?color)
                    :=> (str 'exec.make (str-qt (? nam)) (str-qt (? obj))
                                        ((? size) sizes) (str-qt (? color))))
    ((move-to ?s)   :=> (str 'exec.move-to sp (stack-no (? s))))
    ((drop-at ?s)   :=> (str 'exec.drop-at sp (stack-no (? s))))
    ((pick-from ?s) :=> (str 'exec.pick-from sp (stack-no (? s))))
    ( ?_            :=> (ui-out :dbg 'ERROR '(unknown NetLogo cmd)))
    ))






