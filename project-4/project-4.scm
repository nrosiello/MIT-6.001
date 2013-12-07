;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project 4: Object-oriented Adventure Game
;; Nick Rosiello
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "project-4/objsys.scm")
(load "project-4/objtypes.scm")
(load "project-4/setup.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Warmup 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; me: an avatar
;; (ask me 'LOCATION): retuns a place object
;; place objects accept the following messages:  
;;    EXITS, EXIT-TOWARD, ADD EXIT
;;    from the container superclass: THINGS, HAVE-THING?, ADD-THING, DEL-THING
;;    from the named-object superclass: NAME, INSTALL, DESTROY
;;    from the root superclass: IS-A

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Warmup 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; classes defined in objtypes
;; (class): (superclasses)
;; named-object:
;; exit: named-object
;; thing: named-object
;; mobile-thing: thing, named-object
;; spell: mobile-thing, thing, named-object
;; container: root
;; place: container, root, named-object
;; person: container, root, mobile-thing, thing, named-object
;; avatar: person, container, root, mobile-thing, thing, named-object
;; autonomous-person: person, container, root, mobile-thing, thing, named-object 
;; troll: autonomous-person, person, container, root, mobile-thing, thing, named-object 
;; hall-monitor: autonomous-person, person, container, root, mobile-thing, thing, named-object 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Warmup 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; several types of people (i.e. subclasses of person) are in the world:
;; autonomous persons: ben-bitdiddle, alyssa-hacker, course-6-frosh, lambda-man
;; hall-monitors: dr-evil, mr-bigglesworth
;; trolls: grendel, registrar

;; several random things are present in the world. for example: blackboard,
;; blackboard, lovely-trees, flag-pole, tons-of-code, problem-set, recitation-problem,
;; sicp, engineering-book, diploma
    
;; a room is randomly picked for each person to start out in.  the things are
;; placed in the the same room each time.  starting rooms are different between
;; the things.
