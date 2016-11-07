;;;======================================================
;;;   	Decision-making and categorizer system
;;;
;;;     Version 1.0
;;;     It tries to determine string set, with diameter,
;;;	brand and material
;;;     by asking questions,determining the user level.
;;;
;;;     CLIPS Version 6.0
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

(defmodule MAIN (export ?ALL))

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::ask-question (?why ?help ?question ?allowed-values)
   (format t "%s%nDigita la risposta -> " ?question)
   (bind ?answer (read))
   (format t "%n")
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (if (eq ?answer help) then (format t "%n%s%n%n" ?help))
      (if (eq ?answer why) then (format t "%n%s%n%n" ?why))
      (format t "%s%nDigita la risposta -> " ?question)
      (bind ?answer (read))
      (format t "%n")
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)

;;*****************
;;* INITIAL STATE *
;;*****************

(deftemplate MAIN::attribute
   (slot name)
   (slot value)
   (slot certainty (default 100.0)))

(deftemplate MAIN::ret-phase
   (slot current))

(deftemplate MAIN::second-counter
   (slot count (type INTEGER) (range 1 50)))

(defglobal ?*counting_question* = 1)
(defglobal ?*second-counting_question* = 1)

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::ask-question-to-retract ()
   (format t "Digita il valore numerico della domanda da ritrattare : ")
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (if (eq ?answer 0) then (bind ?answer 100))
   (while (not (< ?answer ?*counting_question*)) do
      (format t "%n")
      (format t "Valore errato!%n%n")
      (format t "Digita il valore numerico della domanda da ritrattare : ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
      (if (eq ?answer 0) then (bind ?answer 100)))
   ?answer)

;;****************
;;* MAIN         *
;;****************

(defrule MAIN::start
  (declare (salience 10000))
  =>
  (set-fact-duplication TRUE)
  (set-strategy random)
  (focus QUESTIONS CHOOSE-QUALITIES STRINGS PRINT-RESULTS PRINT-QUESTIONS RETRACTING))

(defrule MAIN::combine-certainties
  (declare (salience 100)
           (auto-focus TRUE))
  ?rem1 <- (attribute (name ?rel) (value ?val) (certainty ?per1))
  ?rem2 <- (attribute (name ?rel) (value ?val) (certainty ?per2))
  (test (neq ?rem1 ?rem2))
  =>
  (retract ?rem1)
  (modify ?rem2 (certainty (/ (- (* 100 (+ ?per1 ?per2)) (* ?per1 ?per2)) 100))))

;;******************
;;* QUESTION RULES *
;;******************

(defmodule QUESTIONS (import MAIN ?ALL) (export ?ALL))

(deftemplate QUESTIONS::question
   (slot attribute (default ?NONE))
   (slot number_question (default 0))
   (slot help (default nil))
   (slot why (default nil))
   (slot the-question (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE))
   (multislot precursors (default ?DERIVE)))
   
(defrule QUESTIONS::ask-a-question
   ?f <- (question (already-asked FALSE)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers)
		   (help ?help)
		   (why ?why)
		   (number_question ?i))
   =>
   (modify ?f (already-asked TRUE) (number_question ?*counting_question*))
   (assert (attribute (name ?the-attribute)
                      (value (ask-question ?why ?help ?the-question ?valid-answers))))
   (bind ?*counting_question* (+ ?*counting_question* 1)))

(defrule QUESTIONS::precursor-is-satisfied
   ?f <- (question (already-asked FALSE)
                   (precursors ?name is ?value $?rest))
         (attribute (name ?name) (value ?value))
   =>
   (if (eq (nth 1 ?rest) and) 
    then (modify ?f (precursors (rest$ ?rest)))
    else (modify ?f (precursors ?rest))))

(defrule QUESTIONS::precursor-is-not-satisfied
   ?f <- (question (already-asked FALSE)
                   (precursors ?name is-not ?value $?rest))
         (attribute (name ?name) (value ~?value))
   =>
   (if (eq (nth 1 ?rest) and) 
    then (modify ?f (precursors (rest$ ?rest)))
    else (modify ?f (precursors ?rest))))

;;********************
;;* STRING QUESTIONS *
;;********************

(defmodule STRING-QUESTIONS (import QUESTIONS ?ALL))

(deffacts STRING-QUESTIONS::question-attributes

;;*********************
;;* INITIAL QUESTIONS *
;;*********************

  (question (attribute age)
            (the-question "Hai più di 18 anni? [si][no]")
            (valid-answers si no))

  (question (attribute livello-prof)
            (precursors age is si)
            (the-question "Suoni a livello professionista, o guadagni suonando? [si][no]")
            (valid-answers si no))
  (question (attribute livello-standard)
            (the-question "Suoni da almeno 3 anni? [si][no]")
            (valid-answers si no))

;;*********************
;;* GENRE QUESTIONS   *
;;*********************

  (question (attribute guitar-type)
            (the-question "Cerchi corde per chitarra elettrica o acustica? [elettrica][acustica]")
            (valid-answers elettrica acustica))

  (question (attribute genre-type-electric)
            (precursors guitar-type is elettrica)
            (the-question "Qual'è il tuo genere preferito? [metal][rock][blues][jazz]")
            (valid-answers metal rock blues jazz))
  (question (attribute genre-type-acoustic)
            (precursors guitar-type is acustica)
            (the-question "Qual'è il tuo genere preferito? [fingerstyle][rock][country][blues]")
            (valid-answers fingerstyle rock blues country))

  (question (attribute second-genre-type-electric-nometal)
            (precursors genre-type-electric is metal)
            (the-question "Hai un altro genere che selezioneresti? [jazz][rock][blues][no]")
            (valid-answers no rock blues jazz))
  (question (attribute second-genre-type-electric-norock)
            (precursors genre-type-electric is rock)
            (the-question "Hai un altro genere che selezioneresti? [metal][jazz][blues][no]")
            (valid-answers metal no blues jazz))
  (question (attribute second-genre-type-electric-noblues)
            (precursors genre-type-electric is blues)
            (the-question "Hai un altro genere che selezioneresti? [metal][rock][jazz][no]")
            (valid-answers metal rock no jazz))
  (question (attribute second-genre-type-electric-nojazz)
            (precursors genre-type-electric is jazz)
            (the-question "Hai un altro genere che selezioneresti? [metal][rock][blues][no]")
            (valid-answers metal rock blues no))

  (question (attribute second-genre-type-acoustic-nofingerstyle)
            (precursors genre-type-acoustic is fingerstyle)
            (the-question "Hai un altro genere che selezioneresti? [blues][rock][country][no]")
            (valid-answers no rock blues country))
  (question (attribute second-genre-type-acoustic-norock)
            (precursors genre-type-acoustic is rock)
            (the-question "Hai un altro genere che selezioneresti? [fingerstyle][blues][country][no]")
            (valid-answers fingerstyle no blues country))
  (question (attribute second-genre-type-acoustic-nocountry)
            (precursors genre-type-acoustic is country)
            (the-question "Hai un altro genere che selezioneresti? [fingerstyle][rock][blues][no]")
            (valid-answers fingerstyle rock blues no))
  (question (attribute second-genre-type-acoustic-noblues)
            (precursors genre-type-acoustic is blues)
            (the-question "Hai un altro genere che selezioneresti? [fingerstyle][rock][country][no]")
            (valid-answers fingerstyle rock no country))

;;*********************
;;*SUB-GENRE QUESTIONS*
;;*********************

  (question (attribute subgenre-metal)
            (precursors livello-standard is si and guitar-type is elettrica and genre-type-electric is metal)
            (the-question "Quale sottogenere del metal suoni maggiormente? [numetal][progmetal][hairmetal][nessuno]")
            (valid-answers numetal progmetal hairmetal nessuno))
  (question (attribute subgenre-rock)
            (precursors livello-standard is si and guitar-type is elettrica and genre-type-electric is rock)
            (the-question "Quale sottogenere del rock suoni maggiormente? [bluesrock][jazzrock][progrock][hardrock][punk][funk][nessuno]")
            (valid-answers bluesrock progrock hardrock punk funk nessuno))
  (question (attribute subgenre-blues)
            (precursors livello-standard is si and guitar-type is elettrica and genre-type-electric is blues)
            (the-question "Quale sottogenere del blues suoni maggiormente? [countryblues][anni40][anni30][nessuno]")
            (valid-answers countryblues anni40 anni30 nessuno))	
  (question (attribute subgenre-jazz)
            (precursors livello-standard is si and guitar-type is elettrica and genre-type-electric is jazz)
            (the-question "Quale sottogenere del jazz suoni maggiormente? [bluesjazz][jamjazz][fusion][nessuno]")
            (valid-answers bluesjazz jamjazz fusion nessuno))

;;*********************
;;* GUITAR QUESTIONS  *
;;*********************

  (question (attribute guitar-cost)
	    (precursors livello-standard is si)
            (the-question "Il valore della chitarra supera i 450 euro? [si][no][why]")
	    (why "Chitarre sia acustiche che elettriche del valore di 450 euro minimo garantiscono componenti di medio/alta qualità, consentendo una più vasta scelta di corde.")
            (valid-answers si no))
  (question (attribute has-bloccacorda)
            (precursors livello-standard is si and guitar-type is elettrica)
            (the-question "La chitarra ha il bloccocorde floyd rose? [si][no][why][help]")
	    (why "Un bloccacorde consente l'uso di qualsiasi tipo di corda, dato che a prescindere dalla qualità delle meccaniche, mantiene in ottima tensione le corde evitando scordature.")
            (help "Il bloccacorde è una componente montata vicino il capotasto, che mediante viti e meccaniche blocca le corde della chitarra nella parte superiore della tastiera.")
	    (valid-answers si no))
  (question (attribute has-meccanicquality)
            (precursors livello-standard is si and guitar-cost is no)
            (the-question "Le componenti meccaniche sono auto-bloccanti? [si][no][why][help]")
	    (why "Tali componenti consentono l'uso di qualsiasi tipo di corda poichè mantengono in ottima tensione le corde evitando scordature.")
            (help "Meccaniche standard con l'aggiunta di un dispositivo per irrigidire il sistema.")
            (valid-answers si no))
  (question (attribute has-pontemobile)
            (precursors livello-standard is si and guitar-type is elettrica)
	    (why "Un ponte mobile è molto più delicato riguardo la gestione delle corde e richiede quindi corde meno spesse rispetto un ponte fisso.")
            (help "Il ponte è la componente collocata sul corpo della chitarra che mantiene in tensione le corde,se mobile può variare di molto tale tensione.")
            (the-question "La chitarra ha il ponte mobile? [si][no][why][help]")
            (valid-answers si no))
  (question (attribute has-leva)
            (precursors livello-standard is si and guitar-type is elettrica)
	    (why "La leva per il tremolo tende col tempo a scordare la chitarra quindi richiede corde non molto sottili.")
            (help "La leva presente non su tutte le chitarre è utilizzata in combinazione col ponte per poter variare la tensione delle corde.")
            (the-question "La chitarra ha la leva per il tremolo? [si][no][why][help]")
            (valid-answers si no))	
  (question (attribute has-scalooped)
            (precursors livello-standard is si and guitar-cost is si)
	    (why "Una tastiera scalooped è molte sensibile alla pressione sulla corda, quindi è preferibile usare corde sottili a corde spesse che potrebbero portare a pressioni eccessive, e quindi stonature.")
            (help "Una tastiera scalloped ha tasti incavati a forma di semicerchio, utile per bending e vibrati oltre a richiedere un minor sforzo.")
            (the-question "La chitarra ha la tastiera scalloped? [si][no][why][help]")
            (valid-answers si no))


;;*********************
;;* PLETTRO QUESTIONS *
;;*********************

  (question (attribute plettro)
            (the-question "Usi il plettro per suonare? [si][no][help][why]")
	    (help "Il plettro è un piccolo strumento utilizzato per sollecitare o pizzicare le corde.")
	    (why "In base alla dimensione il plettro si adatta meglio ad uno stile, per esempio un plettro light è adatto per lo più ad accompagnamenti, quelli heavy per assoli.")
	    (precursors livello-standard is si)
            (valid-answers si no))
  (question (attribute dimension-plettro)
            (precursors livello-standard is si and plettro is si)
	    (help "Il plettro può avere varie dimensioni, principalmente si divide in plettri light/medium/heavy.")
	    (why "In base alla dimensione il plettro si adatta meglio ad uno stile, per esempio un plettro light è adatto per lo più ad accompagnamenti, quelli heavy per assoli.")
            (the-question "Le dimensioni del plettro sono light? [si][no][non_so][help][why]")
            (valid-answers si no non_so))
  (question (attribute dita)
            (precursors livello-standard is si and plettro is no)
	    (help "Strumenti utilizzati con lo stesso scopo dei plettri standard ma si bloccano sulle dita e servono aumentare la sonorità della pizzicata.")
	    (why "Se usati consentono una più vasta scelta di corde.")
            (the-question "Usi plettri per dita? [si][no][help][why]")
            (valid-answers si no))

;;*********************
;;* TIME QUESTIONS    *
;;*********************

  (question (attribute use-time)
	    (precursors age is si)
            (the-question "Dopo massimo 6 mesi cambi corde? [si][no][non_so][why]")
	    (why "Le corde perdono, in base alla marca scelta e al loro diametro , brillantezza dopo circa 3-4 mesi, quindi maggiore è l'uso più si consigliano corde spesse.")
            (valid-answers si no non_so))

;;*********************
;;* SOLO QUESTIONS    *
;;*********************

  (question (attribute do-solo)
	    (precursors age is si)
            (the-question "Esegui molti assoli? [si][no][non_so][why][help]")
	    (help "Un assolo è una sezione solistica soggetta a virtuosismi, e quindi all'uso di varie tecniche specifiche per genere.")
	    (why "Corde con diametro minore risultano molto più facili e adatte a suonare assoli.")
            (valid-answers si no non_so))
  (question (attribute do-style-metal)
            (precursors age is si and genre-type-electric is metal)
            (the-question "Fai uso di tecniche come tapping, power cord o palm mute? [si][no][non_molto][why][help]")
	    (help "Il tapping consiste nell'utilizzare la mano ritmica per suonare delle note direttamente sulla tastiera. Power cord sono accordi molto usati nel metal in combinazione col palm mute (palmo sulle corde).")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il metal.")
            (valid-answers si no non_molto))
  (question (attribute do-style-rock)
            (precursors age is si and genre-type-electric is rock)
            (the-question "Fai uso di tecniche come bending o legato? [si][no][non_molto][why][help]")
	    (help "Il bending consiste nel suonare una nota spingendo verso l'alto,o basso, aumentando cosi la tonalità. Il legato è una tecniche che prevede l'assenza di pause tra due note.")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il rock.")
            (valid-answers si no non_molto))
  (question (attribute do-style-blues)
            (precursors age is si and genre-type-electric is blues)
            (the-question "Fai uso di tecniche come bending, vibrato o slide? [si][no][non_molto][why][help]")
	    (help "Il bending consiste nel suonare una nota spingendo verso l'alto,o basso, aumentando cosi la tonalità. Lo slide è utile per modificare continuamente la tonalità strisciando sulle corde (con plettro o bottleneck).")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il blues.")
            (valid-answers si no non_molto))
  (question (attribute do-style-jazz)
            (precursors age is si and genre-type-electric is jazz)
            (the-question "Fai uso di tecniche come arpeggi, pizzicato o armonici? [si][no][non_molto][why][help]")
	    (help "Gli armonici naturali o artificiali aumentano di un ottava il suono di una nota.")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il jazz.")
            (valid-answers si no non_molto))
  (question (attribute do-style-fingerstyle)
            (precursors age is si and genre-type-acoustic is fingerstyle)
            (the-question "Fai uso di tecniche come bending, vibrato o armonici? [si][no][non_molto][why][help]")
	    (help "Gli armonici naturali o artificiali aumentano di un ottava il suono di una nota. Il bending consiste nel suonare una nota spingendo verso l'alto,o basso, aumentando cosi la tonalità.")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il fingerstyle.")
            (valid-answers si no non_molto))
  (question (attribute do-capotasto-fingerstyle)
            (precursors age is si and genre-type-acoustic is fingerstyle)
            (the-question "Usi spesso il capotasto mobile? [si][no][non_molto][why][help]")
	    (help "Il capotasto mobile è uno strumento meccanico che si aggancia al manico per aumentare/diminuire la spaziatura delle corde.")
	    (why "L'uso del capotasto mobile ,variando continuamente la tensione delle corde, tende a scordare le corde, quindi si consigliano corde meno spesse.")
            (valid-answers si no non_molto))
  (question (attribute do-style-country)
            (precursors age is si and genre-type-acoustic is country)
            (the-question "Fai uso di tecniche come vibrato, arpeggio o bending? [si][no][non_molto][why][help]")
	    (help "Il bending consiste nel suonare una nota spingendo verso l'alto,o basso, aumentando cosi la tonalità.")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il country.")
            (valid-answers si no non_molto))
  (question (attribute do-style-folk)
            (precursors age is si and genre-type-acoustic is folk)
	    (help "Il palm mute è una tecniche utile a modificare la tonalità del suono mantenendo il palmo sulle corde.")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il folk.")
            (the-question "Fai uso della tecnica del palm mute? [si][no][non_molto][why][help]")
            (valid-answers si no non_molto))
  (question (attribute do-style-acousticblues)
            (precursors age is si and genre-type-acoustic is blues)
            (the-question "Fai uso di tecniche come bending, vibrato o slide? [si][no][non_molto][why][help]")
	    (help "Il bending consiste nel suonare una nota spingendo verso l'alto,o basso, aumentando cosi la tonalità. Lo slide è utile per modificare continuamente la tonalità strisciando sulle corde (con plettro o bottleneck).")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il blues.")
            (valid-answers si no non_molto))
  (question (attribute do-style-acousticrock)
            (precursors age is si and genre-type-acoustic is rock)
            (the-question "Fai uso di tecniche come bending o legato? [si][no][non_molto][why][help]")
	    (help "Il bending consiste nel suonare una nota spingendo verso l'alto,o basso, aumentando cosi la tonalità. Il legato è una tecniche che prevede l'assenza di pause tra due note.")
	    (why "L'uso di queste tecniche richiede corde leggermente meno spesse rispetto delle corde standard per il rock.")
            (valid-answers si no non_molto))

;;*********************
;;* SOUND QUESTIONS   *
;;*********************

  (question (attribute sound)
	    (precursors age is si)
            (the-question "Preferisci ottenere un sound molto aggressivo e potente o più morbido e dolce? [aggressivo][morbido][non_so][why][help]")
	    (why "Un suono molto aggressivo si ottiene con corde molto spesse, al contrario un suono morbido si ottiene con corde piuttosto sottili.")
	    (help "Una sonorità aggressiva è tipica ad esempio del metal, una morbida invece è molto tipica di musiche d'accompagnamento, o jazz/blues.")
            (valid-answers aggressivo morbido non_so))

;;*********************
;;* PROF QUESTIONS    *
;;*********************

  (question (attribute registrazione)
	    (precursors livello-prof is si)
            (the-question "Lavori molto in sala di registrazione professionale? [si][no][why]")
	    (why "In sala di registrazione è preferibile usare meno effetti o sonorità in cambio di un minore noise, quindi corde sottili sono ideali.")
            (valid-answers si no))
  (question (attribute noise)
	    (precursors livello-prof is si)
            (the-question "Suoni all'aperto e preferisci diminuire il noise della chitarra? [si][no][non_so][why]")
	    (why "Suonando all'aperto il suono della chitarra non è molto definito, e si può crear molto noise quindi è preferibile usare corde sottili.")	
            (valid-answers si no non_so))
  (question (attribute effetti)
	    (precursors livello-prof is si)
            (the-question "Suoni con molti effetti o pedali attivi? [si][no][why][help]")
	    (why "Molti effetti anche diversi dalle distorsioni, collegati prima dell'amplificatore, portano inevitabilmente a noise..")
	    (help "Un effetto musicale per chitarra è un apparato elettronico che permette di modulare e arricchire il suono della chitarra. Montati ovviamente in catena prima dell'amplificatore.")
            (valid-answers si no))
  (question (attribute distorsione)
	    (precursors livello-prof is si and effetti is si)
            (the-question "Usi molto effetti di distorsione o Overdrive (anche soft)? [si][no][why][help]")
	    (why "Anche un singolo pedale di distorsione, o un canale di distorsione dell'amplificatore, porta noise, talvolta eccessivo.")
	    (help "Simula il naturale effetto di saturazione di uno stadio di preamplificazione in cui viene immesso un segnale a volume troppo alto, creando il classico sound del rock")
            (valid-answers si no))

;;*********************
;;* COST QUESTIONS    *
;;*********************

  (question (attribute costo)
            (the-question "Quanto preferiresti pagare per un set di corde, meno o più di 15 euro? [meno][piu]")
            (valid-answers meno piu))
)

;;******************
;; The RULES module
;;******************

(defmodule RULES (import MAIN ?ALL) (export ?ALL))

(deftemplate RULES::rule
  (slot certainty (default 100.0))
  (multislot if)
  (multislot then))

(defrule RULES::throw-away-ands-in-antecedent
  ?f <- (rule (if and $?rest))
  =>
  (modify ?f (if ?rest)))

(defrule RULES::throw-away-ands-in-consequent
  ?f <- (rule (then and $?rest))
  =>
  (modify ?f (then ?rest)))

(defrule RULES::remove-is-condition-when-satisfied
  ?f <- (rule (certainty ?c1) 
              (if ?attribute is ?value $?rest))
  (attribute (name ?attribute) 
             (value ?value) 
             (certainty ?c2))
  =>
  (modify ?f (certainty (min ?c1 ?c2)) (if ?rest)))

(defrule RULES::remove-is-not-condition-when-satisfied
  ?f <- (rule (certainty ?c1) 
              (if ?attribute is-not ?value $?rest))
  (attribute (name ?attribute) (value ~?value) (certainty ?c2))
  =>
  (modify ?f (certainty (min ?c1 ?c2)) (if ?rest)))

(defrule RULES::perform-rule-consequent-with-certainty
  ?f <- (rule (certainty ?c1) 
              (if) 
              (then ?attribute is ?value with certainty ?c2 $?rest))
  =>
  (modify ?f (then ?rest))
  (assert (attribute (name ?attribute) 
                     (value ?value)
                     (certainty (/ (* ?c1 ?c2) 100)))))

(defrule RULES::perform-rule-consequent-without-certainty
  ?f <- (rule (certainty ?c1)
              (if)
              (then ?attribute is ?value $?rest))
  (test (or (eq (length$ ?rest) 0)
            (neq (nth 1 ?rest) with)))
  =>
  (modify ?f (then ?rest))
  (assert (attribute (name ?attribute) (value ?value) (certainty ?c1)))) 

;;*******************************
;;*CHOOSE STRING QUALITIES RULES*
;;*******************************

(defmodule CHOOSE-QUALITIES (import RULES ?ALL)
                            (import QUESTIONS ?ALL)
                            (import MAIN ?ALL))

(defrule CHOOSE-QUALITIES::startit => (focus RULES))

(deffacts the-string-rules

	;diameter

  (rule (if genre-type-electric is metal)
        (then best-diameter is LM with certainty 50 and
              best-diameter is M with certainty 70 and
              best-diameter is MH with certainty 70))
  (rule (if genre-type-electric is rock)
        (then best-diameter is UL with certainty 60 and
              best-diameter is SL with certainty 70 and
              best-diameter is L with certainty 50))
  (rule (if genre-type-electric is blues)
        (then best-diameter is MH with certainty 50 and
              best-diameter is H with certainty 50))
  (rule (if genre-type-electric is jazz)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 70))
  (rule (if genre-type-acoustic is fingerstyle)
        (then best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 70))
  (rule (if genre-type-acoustic is rock)
        (then best-diameter is LM with certainty 70 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 20))
  (rule (if genre-type-acoustic is blues)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 50))
  (rule (if genre-type-acoustic is country)
        (then best-diameter is LM with certainty 30 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 50))

  (rule (if second-genre-type-electric-nometal is jazz)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-electric-nometal is rock)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 10))
  (rule (if second-genre-type-electric-nometal is blues)
        (then best-diameter is MH with certainty 10 and
              best-diameter is H with certainty 10))
  (rule (if second-genre-type-electric-norock is jazz)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-electric-norock is blues)
        (then best-diameter is MH with certainty 10 and
              best-diameter is H with certainty 10))
  (rule (if second-genre-type-electric-norock is metal)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 30))
  (rule (if second-genre-type-electric-nojazz is metal)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 30))
  (rule (if second-genre-type-electric-nojazz is blues)
        (then best-diameter is MH with certainty 10 and
              best-diameter is H with certainty 10))
  (rule (if second-genre-type-electric-nojazz is rock)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 10))
  (rule (if second-genre-type-electric-noblues is metal)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 30))
  (rule (if second-genre-type-electric-noblues is rock)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 10))
  (rule (if second-genre-type-electric-noblues is jazz)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40))

  (rule (if second-genre-type-acoustic-noblues is rock)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 10))
  (rule (if second-genre-type-acoustic-noblues is fingerstyle)
        (then best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-acoustic-noblues is country)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-acoustic-norock is blues)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-acoustic-norock is fingerstyle)
        (then best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-acoustic-norock is country)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-acoustic-nofingerstyle is blues)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-acoustic-nofingerstyle is rock)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 10))
  (rule (if second-genre-type-acoustic-nofingerstyle is country)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-acoustic-nocountry is blues)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40))
  (rule (if second-genre-type-acoustic-nocountry is rock)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 10))
  (rule (if second-genre-type-acoustic-nocountry is fingerstyle)
        (then best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 40))

  (rule (if subgenre-metal is numetal)
        (then best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 70))
  (rule (if subgenre-metal is progmetal)
        (then best-diameter is LM with certainty 50 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 40))
  (rule (if subgenre-metal is hairmetal)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 50))

  (rule (if subgenre-rock is bluesrock)
        (then best-diameter is UL with certainty 30 and
              best-diameter is SL with certainty 40 and
              best-diameter is L with certainty 50))
  (rule (if subgenre-rock is jazzrock)
        (then best-diameter is UL with certainty 30 and
              best-diameter is SL with certainty 40 and
              best-diameter is L with certainty 60))
  (rule (if subgenre-rock is progrock)
        (then best-diameter is UL with certainty 60 and
              best-diameter is SL with certainty 70 and
              best-diameter is L with certainty 40))
  (rule (if subgenre-rock is hardrock)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 50 and
              best-diameter is L with certainty 70))
  (rule (if subgenre-rock is punk)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 70))
  (rule (if subgenre-rock is funk)
        (then best-diameter is UL with certainty 30 and
              best-diameter is SL with certainty 70 and
              best-diameter is L with certainty 10))

  (rule (if subgenre-blues is countryblues)
        (then best-diameter is MH with certainty 40 and
              best-diameter is H with certainty 60))
  (rule (if subgenre-blues is anni40)
        (then best-diameter is MH with certainty 40 and
              best-diameter is H with certainty 50))
  (rule (if subgenre-blues is anni30)
        (then best-diameter is MH with certainty 50 and
              best-diameter is H with certainty 40))

  (rule (if subgenre-jazz is bluesjazz)
        (then best-diameter is LM with certainty 30 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 70))
  (rule (if subgenre-jazz is jamjazz)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 60))
  (rule (if subgenre-jazz is fusion)
        (then best-diameter is LM with certainty 50 and
              best-diameter is M with certainty 45 and
              best-diameter is MH with certainty 40))

  (rule (if has-bloccacorda is no)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5))
  (rule (if has-meccanicquality is no)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5))
  (rule (if has-pontemobile is si)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 20 and
              best-diameter is L with certainty 15 and
              best-diameter is LM with certainty 10))
  (rule (if has-leva is si)
        (then best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 15 and
              best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 20 and
              best-diameter is H with certainty 10))
  (rule (if has-scalooped is si)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5))
  (rule (if dita is no)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 15 and
              best-diameter is H with certainty 15))
  (rule (if dimension-plettro is no)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 15 and
              best-diameter is H with certainty 15))
  (rule (if dimension-plettro is si)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 20 and
              best-diameter is L with certainty 15 and
              best-diameter is LM with certainty 10))

  (rule (if do-solo is si)
        (then best-diameter is UL with certainty 5 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5))
  (rule (if do-solo is no)
        (then best-diameter is LM with certainty 5 and
              best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 5 and
              best-diameter is H with certainty 5))
  (rule (if do-style-metal is si)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 20))
  (rule (if do-style-metal is no)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 60))
  (rule (if do-style-rock is si)
        (then best-diameter is UL with certainty 40 and
              best-diameter is SL with certainty 50 and
              best-diameter is L with certainty 20))
  (rule (if do-style-rock is no)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 30))
  (rule (if do-style-blues is si)
        (then best-diameter is MH with certainty 50 and
              best-diameter is H with certainty 30))
  (rule (if do-style-blues is no)
        (then best-diameter is MH with certainty 30 and
              best-diameter is H with certainty 50))
  (rule (if do-style-jazz is si)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40))
  (rule (if do-style-jazz is no)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 50))
  (rule (if do-style-fingerstyle is si)
        (then best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 35))
  (rule (if do-style-fingerstyle is no)
        (then best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 50))
  (rule (if do-style-country is si)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 30))
  (rule (if do-style-country is no)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 50))
  (rule (if do-style-acousticblues is si)
        (then best-diameter is LM with certainty 30 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40))
  (rule (if do-style-acousticblues is no)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40))
  (rule (if do-capotasto-fingerstyle is si)
        (then best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 50))
  (rule (if do-style-acousticrock is si)
        (then best-diameter is LM with certainty 50 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 10))
  (rule (if do-style-acousticrock is no)
        (then best-diameter is LM with certainty 30 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 40))

  (rule (if sound is aggressivo)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 15 and
              best-diameter is H with certainty 15))
  (rule (if sound is morbido)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5))

  (rule (if registrazione is si)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5))
  (rule (if noise is si)
        (then best-diameter is UL with certainty 5 and
              best-diameter is SL with certainty 5 and
              best-diameter is L with certainty 3 and
              best-diameter is LM with certainty 3))
  (rule (if effetti is si)
        (then best-diameter is UL with certainty 5 and
              best-diameter is SL with certainty 5 and
              best-diameter is L with certainty 2 and
              best-diameter is LM with certainty 2))
  (rule (if distorsione is si)
        (then best-diameter is UL with certainty 4 and
              best-diameter is SL with certainty 5 and
              best-diameter is L with certainty 3 and
              best-diameter is LM with certainty 2))

	;material

  (rule (if genre-type-electric is metal)
        (then best-material is nickel))
  (rule (if genre-type-electric is rock)
        (then best-material is acciaio))
  (rule (if genre-type-electric is blues)
        (then best-material is nickel))
  (rule (if genre-type-electric is jazz)
        (then best-material is nickel))
  (rule (if genre-type-acoustic is rock)
        (then best-material is ottone))
  (rule (if genre-type-acoustic is fingerstyle)
        (then best-material is bronzo))
  (rule (if genre-type-acoustic is blues)
        (then best-material is bronzo))
  (rule (if genre-type-acoustic is country)
        (then best-material is bronzo))

	;brand

  (rule (if costo is meno)
        (then best-brand is d'addario))
  (rule (if costo is piu and guitar-type is elettrica)
        (then best-brand is elixir))
  (rule (if costo is piu and guitar-type is acustica)
        (then best-brand is martin))
)

(defrule CHOOSE-QUALITIES::starting-rules-focus
  (declare (salience 10))
  ?t <- (ret-phase (current rule-again-phase))
  =>
  (focus RULES)
  (retract ?t))

;;************************
;;*STRING SELECTION RULES*
;;************************

(defmodule STRINGS (import MAIN ?ALL))

(deffacts any-attributes
  (attribute (name best-diameter) (value any))
  (attribute (name best-material) (value any))
  (attribute (name best-brand) (value any)))

(deftemplate STRINGS::string
  (slot name (default ?NONE))
  (multislot diameter (default any))
  (multislot material (default any))
  (multislot brand (default any)))

(deffacts STRINGS::the-string-list 
  (string (name D'addario-[.08/.42]-Acciaio) (diameter UL) (material acciaio) (brand d'addario))
  (string (name D'addario-[.09/.48]-Acciaio) (diameter SL) (material acciaio) (brand d'addario))
  (string (name D'addario-[.10/.48]-Acciaio) (diameter L) (material acciaio) (brand d'addario))
  (string (name D'addario-[.11/.50]-Acciaio) (diameter LM) (material acciaio) (brand d'addario))
  (string (name D'addario-[.11/.54]-Acciaio) (diameter M) (material acciaio) (brand d'addario))
  (string (name D'addario-[.12/.54]-Acciaio) (diameter MH) (material acciaio) (brand d'addario))
  (string (name D'addario-[.13/.58]-Acciaio) (diameter H) (material acciaio) (brand d'addario))

  (string (name D'addario-[.08/.42]-Nickel) (diameter UL) (material nickel) (brand d'addario))
  (string (name D'addario-[.09/.48]-Nickel) (diameter SL) (material nickel) (brand d'addario))
  (string (name D'addario-[.10/.48]-Nickel) (diameter L) (material nickel) (brand d'addario))
  (string (name D'addario-[.11/.50]-Nickel) (diameter LM) (material nickel) (brand d'addario))
  (string (name D'addario-[.11/.54]-Nickel) (diameter M) (material nickel) (brand d'addario))
  (string (name D'addario-[.12/.54]-Nickel) (diameter MH) (material nickel) (brand d'addario))
  (string (name D'addario-[.13/.58]-Nickel) (diameter H) (material nickel) (brand d'addario))

  (string (name D'addario-[.08/.42]-Ottone) (diameter UL) (material ottone) (brand d'addario))
  (string (name D'addario-[.09/.48]-Ottone) (diameter SL) (material ottone) (brand d'addario))
  (string (name D'addario-[.10/.48]-Ottone) (diameter L) (material ottone) (brand d'addario))
  (string (name D'addario-[.11/.50]-Ottone) (diameter LM) (material ottone) (brand d'addario))
  (string (name D'addario-[.11/.54]-Ottone) (diameter M) (material ottone) (brand d'addario))
  (string (name D'addario-[.12/.54]-Ottone) (diameter MH) (material ottone) (brand d'addario))
  (string (name D'addario-[.13/.58]-Ottone) (diameter H) (material ottone) (brand d'addario))

  (string (name D'addario-[.08/.42]-Bronzo) (diameter UL) (material bronzo) (brand d'addario))
  (string (name D'addario-[.09/.48]-Bronzo) (diameter SL) (material bronzo) (brand d'addario))
  (string (name D'addario-[.10/.48]-Bronzo) (diameter L) (material bronzo) (brand d'addario))
  (string (name D'addario-[.11/.50]-Bronzo) (diameter LM) (material bronzo) (brand d'addario))
  (string (name D'addario-[.11/.54]-Bronzo) (diameter M) (material bronzo) (brand d'addario))
  (string (name D'addario-[.12/.54]-Bronzo) (diameter MH) (material bronzo) (brand d'addario))
  (string (name D'addario-[.13/.58]-Bronzo) (diameter H) (material bronzo) (brand d'addario))

  (string (name Martin-[.08/.42]-Bronzo) (diameter UL) (material bronzo) (brand martin))
  (string (name Martin-[.09/.48]-Bronzo) (diameter SL) (material bronzo) (brand martin))
  (string (name Martin-[.10/.48]-Bronzo) (diameter L) (material bronzo) (brand martin))
  (string (name Martin-[.11/.50]-Bronzo) (diameter LM) (material bronzo) (brand martin))
  (string (name Martin-[.11/.54]-Bronzo) (diameter M) (material bronzo) (brand martin))
  (string (name Martin-[.12/.54]-Bronzo) (diameter MH) (material bronzo) (brand martin))
  (string (name Martin-[.13/.58]-Bronzo) (diameter H) (material bronzo) (brand martin))

  (string (name Martin-[.08/.42]-Ottone) (diameter UL) (material ottone) (brand martin))
  (string (name Martin-[.09/.48]-Ottone) (diameter SL) (material ottone) (brand martin))
  (string (name Martin-[.10/.48]-Ottone) (diameter L) (material ottone) (brand martin))
  (string (name Martin-[.11/.50]-Ottone) (diameter LM) (material ottone) (brand martin))
  (string (name Martin-[.11/.54]-Ottone) (diameter M) (material ottone) (brand martin))
  (string (name Martin-[.12/.54]-Ottone) (diameter MH) (material ottone) (brand martin))
  (string (name Martin-[.13/.58]-Ottone) (diameter H) (material ottone) (brand martin))

  (string (name Elixir-[.08/.42]-Nickel) (diameter UL) (material nickel) (brand elixir))
  (string (name Elixir-[.09/.48]-Nickel) (diameter SL) (material nickel) (brand elixir))
  (string (name Elixir-[.10/.48]-Nickel) (diameter L) (material nickel) (brand elixir))
  (string (name Elixir-[.11/.50]-Nickel) (diameter LM) (material nickel) (brand elixir))
  (string (name Elixir-[.11/.54]-Nickel) (diameter M) (material nickel) (brand elixir))
  (string (name Elixir-[.12/.54]-Nickel) (diameter MH) (material nickel) (brand elixir))
  (string (name Elixir-[.13/.58]-Nickel) (diameter H) (material nickel) (brand elixir))

  (string (name Elixir-[.08/.42]-Acciaio) (diameter UL) (material acciaio) (brand elixir))
  (string (name Elixir-[.09/.48]-Acciaio) (diameter SL) (material acciaio) (brand elixir))
  (string (name Elixir-[.10/.48]-Acciaio) (diameter L) (material acciaio) (brand elixir))
  (string (name Elixir-[.11/.50]-Acciaio) (diameter LM) (material acciaio) (brand elixir))
  (string (name Elixir-[.11/.54]-Acciaio) (diameter M) (material acciaio) (brand elixir))
  (string (name Elixir-[.12/.54]-Acciaio) (diameter MH) (material acciaio) (brand elixir))
  (string (name Elixir-[.13/.58]-Acciaio) (diameter H) (material acciaio) (brand elixir))
)
  
(defrule STRINGS::generate-strings
  (string (name ?name)
        (diameter $? ?c $?)
        (material $? ?b $?)
        (brand $? ?s $?))
  (attribute (name best-diameter) (value ?c) (certainty ?certainty-1))
  (attribute (name best-material) (value ?b) (certainty ?certainty-2))
  (attribute (name best-brand) (value ?s) (certainty ?certainty-3))
  =>
  (assert (attribute (name string) (value ?name)
                     (certainty (min ?certainty-1 ?certainty-2 ?certainty-3)))))

;;*****************************
;;*PRINT SELECTED STRING RULES*
;;*****************************

(defmodule PRINT-RESULTS (import MAIN ?ALL))

(defrule PRINT-RESULTS::header ""
   (declare (salience 10))
   =>
   (format t "%nI set consigliati sono i seguenti (con relative percentuali di sicurezza):%n%n")	
   (assert (phase print-strings)))

(defrule PRINT-RESULTS::print-strings ""
  ?rem <- (attribute (name string) (value ?name) (certainty ?per))		  
  (not (attribute (name string) (certainty ?per1&:(> ?per1 ?per))))
  =>
  (retract ?rem)
  (format t "Set di corde : %s       - %d%%%n" ?name ?per))

(defrule PRINT-RESULTS::remove-poor-string-choices ""
  ?rem <- (attribute (name string) (certainty ?per&:(< ?per 20)))
  =>
  (retract ?rem))

(defrule PRINT-RESULTS::end-spaces ""
   (not (attribute (name string)))
   ?a <- (phase print-strings)
   =>
   (format t "%n")
   (retract ?a))

;;*****************************
;;*     PRINT QUESTIONS       *
;;*****************************

(defmodule PRINT-QUESTIONS (import QUESTIONS ?ALL)(import MAIN ?ALL))

(deftemplate PRINT-QUESTIONS::firing-rules-temp
   (slot code))

(defrule PRINT-QUESTIONS::print-initial-message
   (declare (salience 100))
   (not (firing-rules-temp (code 1)))
   (not (ret-phase (current ret)))
   =>
   (format t "%nRagionamento seguito per giungere alla soluzione :%n%n")
   (assert (second-counter (count 1)))
   (assert (firing-rules-temp (code 1))))

(defrule PRINT-QUESTIONS::print-question-with-count
   (declare (salience 10))
   ?j <- (second-counter (count ?i))
   ?f <- (question (already-asked TRUE)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
		   (number_question ?i))
   ?a <- (attribute (name ?the-attribute)
                    (value ?value))
   ?b <- (firing-rules-temp (code 1))
   (not (ret-phase (current ret)))
   =>
   (modify ?j (count (+ ?i 1)))
   (format t "%d - %s - %s%n" ?i ?the-question ?value))

(defrule PRINT-QUESTIONS::ending-print-how
   (not (answer ?))
   (not (ret-phase (current ret)))
   => 
   (format t "%nVuoi ritrattare una risposta data? [si][no] : ")
   (assert (answer (read))))

(defrule PRINT-QUESTIONS::bad-answer
   ?answer <- (answer ~si&~no&~SI&~NO)
   (not (ret-phase (current ret)))
   =>
   (format t "%nInserimento errato%n")
   (retract ?answer))

(defrule PRINT-QUESTIONS::proceed-to-retract
   ?answer <- (answer si|SI)
   ?j <- (second-counter (count ?i))
   ?b <- (firing-rules-temp (code 1))
   (not (ret-phase (current ret)))
   =>
   (format t "%n")
   (retract ?answer ?j ?b)
   (assert (ret-phase (current ret))))

(defrule PRINT-QUESTIONS::proceed-to-halt
   ?answer <- (answer no|NO)
   ?j <- (second-counter (count ?i))
   ?b <- (firing-rules-temp (code 1))
   (not (ret-phase (current ret)))
   =>
   (format t "%n")
   (retract ?answer ?j ?b)
   (halt))

;;*****************************
;;*     RETRACTING PHASE      *
;;*****************************

(defmodule RETRACTING (import QUESTIONS ?ALL)(import MAIN ?ALL)(import RULES ?ALL))

(deftemplate RETRACTING::indice
   (slot count))

(deftemplate RETRACTING::original-attribute
   (slot name)
   (slot value))

(deftemplate RETRACTING::original-question
   (slot attribute (default ?NONE))
   (multislot precursors (default ?DERIVE)))

(deftemplate RETRACTING::question-to-ask-after-retract
   (slot question)
   (slot attribute))

(defrule RETRACTING::print-retracting-question
   (declare (salience 100))
   (ret-phase (current ret))
   => 
   (assert (indice (count (ask-question-to-retract)))))

(defrule RETRACTING::save-retracted-question
   (declare (salience 90))
   ?a <- (ret-phase (current ret))
   (indice (count ?i))
   (question (attribute ?the-attribute)
	     (number_question ?i)
	     (the-question ?question))
   =>
   (modify ?a (current retracting))
   (assert (question-to-ask-after-retract (question ?question) (attribute ?the-attribute))))

(defrule RETRACTING::retracting-things
   (ret-phase (current retracting))
   ?a <- (phase print-strings)
   =>
   (retract ?a))

(defrule RETRACTING::retracting-rules
   (declare (salience 100))
   (ret-phase (current retracting))
   ?r <- (rule (certainty ?y))
   =>
   (retract ?r))

(defrule RETRACTING::retracting-attributes
   (declare (salience 95))
   (ret-phase (current retracting))
   ?a <- (attribute (name ?y) (value ?v))
   =>
   (assert (original-attribute (name ?y) (value ?v)))
   (retract ?a))

(defrule RETRACTING::asserting-original-question
   (declare (salience 90))
   ?a <- (ret-phase (current retracting))
   =>
   (bind ?*counting_question* (- ?*counting_question* 1))
   (modify ?a (current doing-retract))
;;*********************
;;* ORIGINAL QUESTIONS*
;;*********************

;;*********************
;;* INITIAL QUESTIONS *
;;*********************

  (assert (original-question (attribute age)))

  (assert (original-question (attribute livello-prof)
            (precursors age is si)))
  (assert (original-question (attribute livello-standard)))

;;*********************
;;* GENRE QUESTIONS   *
;;*********************

  (assert (original-question (attribute guitar-type)))

  (assert (original-question (attribute genre-type-electric)
            (precursors guitar-type is elettrica)))
  (assert (original-question (attribute genre-type-acoustic)
            (precursors guitar-type is acustica)))

  (assert (original-question (attribute second-genre-type-electric-nometal)
            (precursors genre-type-electric is metal)))
  (assert (original-question (attribute second-genre-type-electric-norock)
            (precursors genre-type-electric is rock)))
  (assert (original-question (attribute second-genre-type-electric-noblues)
            (precursors genre-type-electric is blues)))
  (assert (original-question (attribute second-genre-type-electric-nojazz)
            (precursors genre-type-electric is jazz)))

  (assert (original-question (attribute second-genre-type-acoustic-nofingerstyle)
            (precursors genre-type-acoustic is fingerstyle)))
  (assert (original-question (attribute second-genre-type-acoustic-norock)
            (precursors genre-type-acoustic is rock)))
  (assert (original-question (attribute second-genre-type-acoustic-nocountry)
            (precursors genre-type-acoustic is country)))
  (assert (original-question (attribute second-genre-type-acoustic-noblues)
            (precursors genre-type-acoustic is blues)))

;;*********************
;;*SUB-GENRE QUESTIONS*
;;*********************

  (assert (original-question (attribute subgenre-metal)
            (precursors livello-standard is si and guitar-type is elettrica and genre-type-electric is metal)))
  (assert (original-question (attribute subgenre-rock)
            (precursors livello-standard is si and guitar-type is elettrica and genre-type-electric is rock)))
  (assert (original-question (attribute subgenre-blues)
            (precursors livello-standard is si and guitar-type is elettrica and genre-type-electric is blues))	)
  (assert (original-question (attribute subgenre-jazz)
            (precursors livello-standard is si and guitar-type is elettrica and genre-type-electric is jazz)))

;;*********************
;;* GUITAR QUESTIONS  *
;;*********************

  (assert (original-question (attribute guitar-cost)
	    (precursors livello-standard is si)))
  (assert (original-question (attribute has-bloccacorda)
            (precursors livello-standard is si and guitar-type is elettrica)))
  (assert (original-question (attribute has-meccanicquality)
            (precursors livello-standard is si and guitar-cost is no)))
  (assert (original-question (attribute has-pontemobile)
            (precursors livello-standard is si and guitar-type is elettrica)))
  (assert (original-question (attribute has-leva)
            (precursors livello-standard is si and guitar-type is elettrica)))	
  (assert (original-question (attribute has-scalooped)
            (precursors livello-standard is si and guitar-cost is si)))


;;*********************
;;* PLETTRO QUESTIONS *
;;*********************

  (assert (original-question (attribute plettro)
	    (precursors livello-standard is si)))
  (assert (original-question (attribute dimension-plettro)
            (precursors livello-standard is si and plettro is si)))
  (assert (original-question (attribute dita)
            (precursors livello-standard is si and plettro is no)))

;;*********************
;;* TIME QUESTIONS    *
;;*********************

  (assert (original-question (attribute use-time)
	    (precursors age is si)))

;;*********************
;;* SOLO QUESTIONS    *
;;*********************

  (assert (original-question (attribute do-solo)
	    (precursors age is si)))
  (assert (original-question (attribute do-style-metal)
            (precursors age is si and genre-type-electric is metal)))
  (assert (original-question (attribute do-style-rock)
            (precursors age is si and genre-type-electric is rock)))
  (assert (original-question (attribute do-style-blues)
            (precursors age is si and genre-type-electric is blues)))
  (assert (original-question (attribute do-style-jazz)
            (precursors age is si and genre-type-electric is jazz)))
  (assert (original-question (attribute do-style-fingerstyle)
            (precursors age is si and genre-type-acoustic is fingerstyle)))
  (assert (original-question (attribute do-capotasto-fingerstyle)
            (precursors age is si and genre-type-acoustic is fingerstyle)))
  (assert (original-question (attribute do-style-country)
            (precursors age is si and genre-type-acoustic is country)))
  (assert (original-question (attribute do-style-folk)
            (precursors age is si and genre-type-acoustic is folk)))
  (assert (original-question (attribute do-style-acousticblues)
            (precursors age is si and genre-type-acoustic is blues)))
  (assert (original-question (attribute do-style-acousticrock)
            (precursors age is si and genre-type-acoustic is rock)))

;;*********************
;;* SOUND QUESTIONS   *
;;*********************

  (assert (original-question (attribute sound)
	    (precursors age is si)))

;;*********************
;;* PROF QUESTIONS    *
;;*********************

  (assert (original-question (attribute registrazione)
	    (precursors livello-prof is si)))
  (assert (original-question (attribute noise)
	    (precursors livello-prof is si)))
  (assert (original-question (attribute effetti)
	    (precursors livello-prof is si)))
  (assert (original-question (attribute distorsione)
	    (precursors livello-prof is si and effetti is si)))

;;*********************
;;* COST QUESTIONS    *
;;*********************

  (assert (original-question (attribute costo)))
  )

(defrule RETRACTING::changing-question
   (declare (salience 80))
   (ret-phase (current doing-retract))
   ?y <- (indice (count ?i))
   ?f <- (question (already-asked TRUE)
                   (attribute ?the-attribute)
		   (number_question ?i))
   ?t <- (original-question (attribute ?the-attribute)
                            (precursors $?all-precursors))
  =>
  (modify ?f (already-asked FALSE) (precursors ?all-precursors) (number_question 0))
  (bind ?*counting_question* (- ?*counting_question* 1))
  (modify ?y (count (+ ?i 1))))

(defrule RETRACTING::clear-original-question
  (declare (salience 50))
  (ret-phase (current doing-retract))
  ?t <- (original-question (attribute ?the-attribute)
                            (precursors $?all-precursors))
  =>
  (retract ?t))

(defrule RETRACTING::changing-phase
  ?t <- (ret-phase (current doing-retract))
  ?y <- (indice (count ?i))
  =>
  (modify ?y (count 1))
  (modify ?t (current clearing-attribute)))

(defrule RETRACTING::reset-attribute
  (declare (salience 40))
  (ret-phase (current clearing-attribute))
  ?y <- (indice (count ?i))
  (question (already-asked TRUE)
            (attribute ?the-attribute)
	    (number_question ?i))
  ?t <- (original-attribute (name ?the-attribute)
                            (value ?value))
  =>
  (modify ?y (count (+ ?i 1)))
  (assert (attribute (name ?the-attribute) (value ?value))))

(defrule RETRACTING::clear-original-attribute
  (declare (salience 30))
  (ret-phase (current clearing-attribute))
  ?t <- (original-attribute (name ?the-attribute)
                            (value ?value))
  =>
  (retract ?t))

(defrule RETRACTING::reset-starting-attribute
  (declare (salience 20))
  (ret-phase (current clearing-attribute))
  =>
  (assert (attribute (name best-diameter) (value any)))
  (assert (attribute (name best-material) (value any)))
  (assert (attribute (name best-brand) (value any))))


(defrule RETRACTING::reset-starting-rules
  (declare (salience 10))
  (ret-phase (current clearing-attribute))
  =>
  	;diameter

  (assert (rule (if genre-type-electric is metal)
        (then best-diameter is LM with certainty 50 and
              best-diameter is M with certainty 70 and
              best-diameter is MH with certainty 70)))
  (assert (rule (if genre-type-electric is rock)
        (then best-diameter is UL with certainty 60 and
              best-diameter is SL with certainty 70 and
              best-diameter is L with certainty 50)))
  (assert (rule (if genre-type-electric is blues)
        (then best-diameter is MH with certainty 50 and
              best-diameter is H with certainty 50)))
  (assert (rule (if genre-type-electric is jazz)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 70)))
  (assert (rule (if genre-type-acoustic is fingerstyle)
        (then best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 70)))
  (assert (rule (if genre-type-acoustic is rock)
        (then best-diameter is LM with certainty 70 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 20)))
  (assert (rule (if genre-type-acoustic is blues)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 50)))
  (assert (rule (if genre-type-acoustic is country)
        (then best-diameter is LM with certainty 30 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 50)))

  (assert (rule (if second-genre-type-electric-nometal is jazz)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-electric-nometal is rock)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 10)))
  (assert (rule (if second-genre-type-electric-nometal is blues)
        (then best-diameter is MH with certainty 10 and
              best-diameter is H with certainty 10)))
  (assert (rule (if second-genre-type-electric-norock is jazz)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-electric-norock is blues)
        (then best-diameter is MH with certainty 10 and
              best-diameter is H with certainty 10)))
  (assert (rule (if second-genre-type-electric-norock is metal)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 30)))
  (assert (rule (if second-genre-type-electric-nojazz is metal)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 30)))
  (assert (rule (if second-genre-type-electric-nojazz is blues)
        (then best-diameter is MH with certainty 10 and
              best-diameter is H with certainty 10)))
  (assert (rule (if second-genre-type-electric-nojazz is rock)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 10)))
  (assert (rule (if second-genre-type-electric-noblues is metal)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 30)))
  (assert (rule (if second-genre-type-electric-noblues is rock)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 10)))
  (assert (rule (if second-genre-type-electric-noblues is jazz)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40)))

  (assert (rule (if second-genre-type-acoustic-noblues is rock)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 10)))
  (assert (rule (if second-genre-type-acoustic-noblues is fingerstyle)
        (then best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-acoustic-noblues is country)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-acoustic-norock is blues)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-acoustic-norock is fingerstyle)
        (then best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-acoustic-norock is country)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-acoustic-nofingerstyle is blues)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-acoustic-nofingerstyle is rock)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 10)))
  (assert (rule (if second-genre-type-acoustic-nofingerstyle is country)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-acoustic-nocountry is blues)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if second-genre-type-acoustic-nocountry is rock)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 10)))
  (assert (rule (if second-genre-type-acoustic-nocountry is fingerstyle)
        (then best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 40)))

  (assert (rule (if subgenre-metal is numetal)
        (then best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 70)))
  (assert (rule (if subgenre-metal is progmetal)
        (then best-diameter is LM with certainty 50 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if subgenre-metal is hairmetal)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 50)))

  (assert (rule (if subgenre-rock is bluesrock)
        (then best-diameter is UL with certainty 30 and
              best-diameter is SL with certainty 40 and
              best-diameter is L with certainty 50)))
  (assert (rule (if subgenre-rock is jazzrock)
        (then best-diameter is UL with certainty 30 and
              best-diameter is SL with certainty 40 and
              best-diameter is L with certainty 60)))
  (assert (rule (if subgenre-rock is progrock)
        (then best-diameter is UL with certainty 60 and
              best-diameter is SL with certainty 70 and
              best-diameter is L with certainty 40)))
  (assert (rule (if subgenre-rock is hardrock)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 50 and
              best-diameter is L with certainty 70)))
  (assert (rule (if subgenre-rock is punk)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 70)))
  (assert (rule (if subgenre-rock is funk)
        (then best-diameter is UL with certainty 30 and
              best-diameter is SL with certainty 70 and
              best-diameter is L with certainty 10)))

  (assert (rule (if subgenre-blues is countryblues)
        (then best-diameter is MH with certainty 40 and
              best-diameter is H with certainty 60)))
  (assert (rule (if subgenre-blues is anni40)
        (then best-diameter is MH with certainty 40 and
              best-diameter is H with certainty 50)))
  (assert (rule (if subgenre-blues is anni30)
        (then best-diameter is MH with certainty 50 and
              best-diameter is H with certainty 40)))

  (assert (rule (if subgenre-jazz is bluesjazz)
        (then best-diameter is LM with certainty 30 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 70)))
  (assert (rule (if subgenre-jazz is jamjazz)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 60)))
  (assert (rule (if subgenre-jazz is fusion)
        (then best-diameter is LM with certainty 50 and
              best-diameter is M with certainty 45 and
              best-diameter is MH with certainty 40)))

  (assert (rule (if has-bloccacorda is no)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5)))
  (assert (rule (if has-meccanicquality is no)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5)))
  (assert (rule (if has-pontemobile is si)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 20 and
              best-diameter is L with certainty 15 and
              best-diameter is LM with certainty 10)))
  (assert (rule (if has-leva is si)
        (then best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 15 and
              best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 20 and
              best-diameter is H with certainty 10)))
  (assert (rule (if has-scalooped is si)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5)))
  (assert (rule (if dita is no)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 15 and
              best-diameter is H with certainty 15)))
  (assert (rule (if dimension-plettro is no)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 15 and
              best-diameter is H with certainty 15)))
  (assert (rule (if dimension-plettro is si)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 20 and
              best-diameter is L with certainty 15 and
              best-diameter is LM with certainty 10)))

  (assert (rule (if do-solo is si)
        (then best-diameter is UL with certainty 5 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5)))
  (assert (rule (if do-solo is no)
        (then best-diameter is LM with certainty 5 and
              best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 5 and
              best-diameter is H with certainty 5)))
  (assert (rule (if do-style-metal is si)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 20)))
  (assert (rule (if do-style-metal is no)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 50 and
              best-diameter is MH with certainty 60)))
  (assert (rule (if do-style-rock is si)
        (then best-diameter is UL with certainty 40 and
              best-diameter is SL with certainty 50 and
              best-diameter is L with certainty 20)))
  (assert (rule (if do-style-rock is no)
        (then best-diameter is UL with certainty 20 and
              best-diameter is SL with certainty 30 and
              best-diameter is L with certainty 30)))
  (assert (rule (if do-style-blues is si)
        (then best-diameter is MH with certainty 50 and
              best-diameter is H with certainty 30)))
  (assert (rule (if do-style-blues is no)
        (then best-diameter is MH with certainty 30 and
              best-diameter is H with certainty 50)))
  (assert (rule (if do-style-jazz is si)
        (then best-diameter is LM with certainty 40 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if do-style-jazz is no)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 50)))
  (assert (rule (if do-style-fingerstyle is si)
        (then best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 35)))
  (assert (rule (if do-style-fingerstyle is no)
        (then best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 50)))
  (assert (rule (if do-style-country is si)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 30)))
  (assert (rule (if do-style-country is no)
        (then best-diameter is LM with certainty 20 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 50)))
  (assert (rule (if do-style-acousticblues is si)
        (then best-diameter is LM with certainty 30 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if do-style-acousticblues is no)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 30 and
              best-diameter is MH with certainty 40)))
  (assert (rule (if do-capotasto-fingerstyle is si)
        (then best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 50)))
  (assert (rule (if do-style-acousticrock is si)
        (then best-diameter is LM with certainty 50 and
              best-diameter is M with certainty 20 and
              best-diameter is MH with certainty 10)))
  (assert (rule (if do-style-acousticrock is no)
        (then best-diameter is LM with certainty 30 and
              best-diameter is M with certainty 40 and
              best-diameter is MH with certainty 40)))

  (assert (rule (if sound is aggressivo)
        (then best-diameter is LM with certainty 10 and
              best-diameter is M with certainty 10 and
              best-diameter is MH with certainty 15 and
              best-diameter is H with certainty 15)))
  (assert (rule (if sound is morbido)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5)))

  (assert (rule (if registrazione is si)
        (then best-diameter is UL with certainty 10 and
              best-diameter is SL with certainty 10 and
              best-diameter is L with certainty 5 and
              best-diameter is LM with certainty 5)))
  (assert (rule (if noise is si)
        (then best-diameter is UL with certainty 5 and
              best-diameter is SL with certainty 5 and
              best-diameter is L with certainty 3 and
              best-diameter is LM with certainty 3)))
  (assert (rule (if effetti is si)
        (then best-diameter is UL with certainty 5 and
              best-diameter is SL with certainty 5 and
              best-diameter is L with certainty 2 and
              best-diameter is LM with certainty 2)))
  (assert (rule (if distorsione is si)
        (then best-diameter is UL with certainty 4 and
              best-diameter is SL with certainty 5 and
              best-diameter is L with certainty 3 and
              best-diameter is LM with certainty 2)))

	;material

  (assert (rule (if genre-type-electric is metal)
        (then best-material is nickel)))
  (assert (rule (if genre-type-electric is rock)
        (then best-material is acciaio)))
  (assert (rule (if genre-type-electric is blues)
        (then best-material is nickel)))
  (assert (rule (if genre-type-electric is jazz)
        (then best-material is nickel)))
  (assert (rule (if genre-type-acoustic is rock)
        (then best-material is ottone)))
  (assert (rule (if genre-type-acoustic is fingerstyle)
        (then best-material is bronzo)))
  (assert (rule (if genre-type-acoustic is blues)
        (then best-material is bronzo)))
  (assert (rule (if genre-type-acoustic is country)
        (then best-material is bronzo)))

	;brand

  (assert (rule (if costo is meno)
        (then best-brand is d'addario)))
  (assert (rule (if costo is piu and guitar-type is elettrica)
        (then best-brand is elixir)))
  (assert (rule (if costo is piu and guitar-type is acustica)
        (then best-brand is martin))))

(defrule RETRACTING::do-retracted-question
  (declare (salience 5))
  (ret-phase (current clearing-attribute))
  ?f <- (question (already-asked FALSE)
                  (the-question ?the-question)
                  (attribute ?the-attribute)
                  (valid-answers $?valid-answers)
		  (help ?help)
		  (why ?why)
		  (number_question ?i))
  ?t <- (question-to-ask-after-retract (question ?the-question) (attribute ?the-attribute))
  =>
  (bind ?*counting_question* (+ ?*counting_question* 1))
  (modify ?f (already-asked TRUE) (number_question ?*counting_question* ))
  (assert (attribute (name ?the-attribute)
                     (value (ask-question ?why ?help ?the-question ?valid-answers))))
  (retract ?t))

(defrule RETRACTING::ending-general-retract
  ?t <- (ret-phase (current clearing-attribute))
  ?y <- (indice (count ?i))
  =>
  (bind ?*counting_question* (+ ?*counting_question* 1))
  (modify ?t (current rule-again-phase))
  (retract ?y)
  (focus QUESTIONS CHOOSE-QUALITIES STRINGS PRINT-RESULTS PRINT-QUESTIONS RETRACTING))
