(in-package :aoc-2022)

(defun parse-cost ()
  (with-monad
    (assign costs
            (parse-list (with-monad
                          (assign num (parse-number))
                          (parse-space)
                          (assign type (parse-keyword #'alphanumericp))
                          (unit (make-mineral type num)))
                        " and "))
    (unit (reduce #'mineral+ costs))))

(defun parse-robot ()
  (with-monad
    (parse-string "Each ")
    (assign type (parse-word #'alphanumericp))
    (parse-string " robot costs ")
    (assign cost (parse-cost))
    (parse-string ".")
    (unit (list (intern (string-upcase type)) cost))))

(defun parse-blueprint ()
  (with-monad
    (parse-string "Blueprint ")
    (assign id (parse-number))
    (parse-string ": ")
    (assign robots (parse-list  (parse-robot) " "))
    (unit (list id robots))))

(defun parse-file ()
  (parse-lines (parse-blueprint)))

(defparameter *mineral-types* '(ore clay obsidian geode))
(defstruct mineral
  (ore 0) (clay 0) (obsidian 0) (geode 0))
(defparameter *robots*
  (iter
    (with ret = (make-hash-table))
    (for type in *mineral-types*)
    (setf (gethash type ret)
	  (make-mineral (intern (symbol-name type) :keyword) 1))
    (finally (return ret))))

(defun map-mineral (fn)
  (lambda (a b)
    (iter
      (with ret = (make-mineral))
      (for slot in '(ore clay obsidian geode))
      (setf (slot-value ret slot)
	    (funcall fn (slot-value a slot) (slot-value b slot)))
      (finally (return ret)))))

(setf (symbol-function 'mineral+) (map-mineral #'+))
(setf (symbol-function 'mineral-) (map-mineral #'-))
(setf (symbol-function 'mineral-max) (map-mineral #'max))
(defun mineral* (m k)
  (let ((ret (make-mineral)))
    (mapcar (lambda (slot) (setf (slot-value ret slot)
				 (* k (slot-value m slot))))
	    *mineral-types*)
    ret))

(defun required-resources (robot-type blueprint)
  (iter
    (for (type resources) in blueprint)
    (finding resources such-that (eq robot-type type))))

;; Return the number of minutes until required resources can be collected
;; NIL if we can't.
(defun time-to-collect (required resources robots)
  (labels ((have-production (required robots)
	     (every (lambda (slot)
		      (or (<= (slot-value required slot) 0)
			  (> (slot-value robots slot) 0)))
		    *mineral-types*)))
    (let ((required (mineral- required resources)))
      (when (have-production required robots)
	(iter
	  (for type in *mineral-types*)
	  (maximizing (if (<= (slot-value required type) 0)
			  0
			  (ceiling (slot-value required type)
				   (slot-value robots type)))))))))

;; Return list of robots to build. Each item is a list of robot type, when it'll
;; be ready, and the required resources.
;; Don't return robots if we're already at max production (apart from geode)
;; Don't return robots if they won't produce anything (too little time)
(defun choose-robot-to-build (time-remaining resources robots blueprint max-production)
  (iter
    (for type in *mineral-types*)
    (for required-resource = (required-resources type blueprint))
    (for time-to-collect = (time-to-collect required-resource resources robots))
    (when (and (or (eq type 'geode)
		   (< (slot-value robots type)
                      (slot-value max-production type)))
	       (and time-to-collect (<= time-to-collect (- time-remaining 2))))
      (collect (list type (1+ time-to-collect) required-resource)))))

;; How many resources we need to be able to build any robot we like
(defun max-required-production (blueprint)
  (reduce (lambda (ret type)
	    (mineral-max ret (required-resources type blueprint)))
	  *mineral-types*
	  :initial-value (make-mineral)))

;; Upper bound on the number of geodes we can make (disregarding resources)
;; How many we have + (Geode robots * time) + (Make a Geode robot every min)
(defun upper-bound (time-remaining resources robots)
  (+ (mineral-geode resources)
     (* time-remaining (mineral-geode robots))
     (/ (* time-remaining (1- time-remaining)) 2)))

(defun max-geodes (time-remaining resources robots blueprint max-resources best-so-far)
  (cond
    ((< (upper-bound time-remaining resources robots) best-so-far) 0)
    ((= 0 time-remaining) (mineral-geode resources))
    (t (let ((options (choose-robot-to-build time-remaining
					     resources
					     robots
					     blueprint
					     max-resources)))
	 (if (null options)
	     (+ (mineral-geode resources)
		(* (mineral-geode robots) time-remaining))
	     (iter
	       (for (type build-time required-resource) in options)
	       (for new-resources =
		    (mineral- (mineral+ resources (mineral* robots build-time))
			      required-resource))
	       (for new-robots = (mineral+ robots (gethash type *robots*)))
	       (maximizing
		(max-geodes (- time-remaining build-time)
			    new-resources
			    new-robots
			    blueprint
			    max-resources
			    best-so-far)
		into best-child)
	       (setf best-so-far (max best-so-far (or best-child 0)))
	       (finally (return best-child))))))))

(defun day19 (input &key (part 1))
  (let ((parsed (run-parser (parse-file) input)))
    (if (= part 1)
	(iter
	  (for (id blueprint) in parsed)
	  (for max-production = (max-required-production blueprint))
	  (for geodes = (max-geodes 24
                                    (make-mineral)
                                    (make-mineral :ore 1)
                                    blueprint
                                    max-production
				    0))
	  (format t "~a ~a ~a~%"  id geodes (* id geodes))
	  (summing (* id geodes)))
	(iter
	  (repeat 3)
	  (for (id blueprint) in parsed)
	  (for max-production = (max-required-production blueprint))
	  (for geodes = (max-geodes 32
				    (make-mineral)
				    (make-mineral :ore 1)
				    blueprint
				    max-production
				    0))
	  (format t "~a ~a ~a~%"  id geodes (* id geodes))
	  (multiply geodes)))))
