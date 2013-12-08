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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to get to the thing part, you ahve to go up the inheritance hierarchy from
;; the person part, to the mobile-thing part, to the thing part.
;; output from showing the thing part:
; HANDLER: #[compound-procedure 34 handler]
; TYPE: thing
;(methods (install #[compound-procedure 38])
;                  (location #[compound-procedure 37])
;                  (destroy #[compound-procedure 36])
;                  (emit #[compound-procedure 35]))
; Parent frame: #[environment 39]
; named-part:   #[compound-procedure 40 handler]
;    Parent frame: global-environment 
;    self:         (instance #[compound-procedure 13 handler])
;    name:         nick
;    location:     (instance #[compound-procedure 19 handler])

;; after moving from the birthplace of the avatar, the location of the
;; mobile-thing changes to the new location.

;; the value of the self variable for the superclass handlers of the avatar class
;; does not ever change, it always points to the base avatar class instance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; testing of two new methods for people:
;; HAS-A method to determine if a person has any things of a given type
;; HAS-A-THING-NAMED method to determine if person has things with a given name
(begin
  (setup 'nick)
  (define loc (ask me 'location))
  (define bob (create-person 'bob loc))
  (define spell-1 (create-spell 'spell-1 loc '() '()))
  (define spell-2 (create-spell 'spell-2 loc '() '()))

  ;; test that bob initially has no spells
  (test-equal (ask bob 'HAS-A 'spell) '())

  (ask bob 'ADD-THING spell-1)
  (ask bob 'ADD-THING spell-2)
  
  ;; test that now bob has spells
  (test-equal (ask bob 'HAS-A 'spell) (list spell-2 spell-1))
  (test-equal (ask bob 'HAS-A 'fake-type) (list))

  ;; test has-a-thing-named method
  (test-equal (ask bob 'HAS-A-THING-NAMED 'fake-name) (list))
  (test-equal (ask bob 'HAS-A-THING-NAMED 'spell-1) (list spell-1))
)
