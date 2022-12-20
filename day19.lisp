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

(defstruct mineral
  (ore 0) (clay 0) (obsidian 0) (geode 0))

(defparameter *mineral-types* '(ore clay obsidian geode))
(defparameter *cache* (make-hash-table :test 'equalp))
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
      (setf (slot-value ret slot) (funcall fn
                                           (slot-value a slot)
                                           (slot-value b slot)))
      (finally (return ret)))))

(setf (symbol-function 'mineral+) (map-mineral #'+))
(setf (symbol-function 'mineral-) (map-mineral #'-))
(setf (symbol-function 'mineral-max) (map-mineral #'max))
(setf (symbol-function 'mineral-clamp)
      (map-mineral (lambda (a b) (if (> b 0) (min a (* 2 b)) a))))

(setf (symbol-function 'mineral>=-int) (map-mineral #'>=))
(defun mineral>= (a b)
  (let ((ret (mineral>=-int a b)))
    (every (lambda (slot) (slot-value ret slot)) *mineral-types*)))

(defun required-resources (robot-type blueprint)
  (iter
    (for (type resources) in blueprint)
    (finding resources such-that (eq robot-type type))))

;; only build a robot if production of that ore type is less than the maximum
;;   required production of that ore type
(defun available-to-build (resources robots blueprint max-production)
  (iter
    (for type in *mineral-types*)
    (for required-resources = (required-resources type blueprint))
    (when (and (mineral>= resources required-resources)
               (or (eq type 'geode)
                   (< (slot-value robots type)
                      (slot-value max-production type))))
      (collect type))))

;; How many resources we need to be able to build any robot we like
(defun max-required-production (blueprint)
  (iter
    (with ret = (make-mineral))
    (for type in *mineral-types*)
    (for required-resources = (required-resources type blueprint))
    (setf ret (mineral-max ret required-resources))
    (finally (return ret))))

;; Upper bound on the number of geodes we can make (disregarding resources)
;; How many we have + (Geode robots * time) + (Make a Geode robot every min)
(defun upper-bound (time-remaining resources robots)
  (+ (mineral-geode resources)
     (* time-remaining (mineral-geode robots))
     (/ (* time-remaining (1- time-remaining)) 2)))

;; Find the max-geodes by building any robot available.
(defun max-geodes-rec (time-remaining resources new-resources robots blueprint max-resources best-so-far)
  (iter
    (for type in (available-to-build resources robots blueprint max-resources))
    (for required-resources = (required-resources type blueprint))
    (for child-resources = (mineral- new-resources required-resources))
    (for child-robots = (mineral+ robots (gethash type *robots*)))
    (maximizing 
     (max-geodes
      (1- time-remaining)
      (mineral-clamp child-resources max-resources)
      (mineral-clamp child-robots max-resources)
      blueprint
      max-resources
      best-so-far)
     into best-child)
    (setf best-so-far (max best-child best-so-far))
    (finally (return (or best-child 0)))))

(defun max-geodes (time-remaining resources robots blueprint max-resources best-so-far)
  (cond
    ((< (upper-bound time-remaining resources robots) best-so-far) 0)
    ((gethash (list time-remaining resources robots) *cache*)
     (gethash (list time-remaining resources robots) *cache*))
    ((= 0 time-remaining) (mineral-geode resources))
    (t (let* ((new-resources  (mineral+ resources robots))
	      (child-best (max-geodes-rec time-remaining
					  resources
					  new-resources
					  robots
					  blueprint
					  max-resources
					  best-so-far))
	      (wait-best (max-geodes (1- time-remaining)
				     (mineral-clamp new-resources max-resources)
				     robots
				     blueprint
				     max-resources
				     (max best-so-far child-best))))
	 (setf (gethash (list time-remaining resources robots) *cache*)
	       (max (gethash (list time-remaining resources robots) *cache* 0)
		    child-best
		    wait-best))))))

(defun day19 (input &key (part 1))
  (let ((parsed (run-parser (parse-file) input)))
    (if (= part 1)
	(iter
	  (for (id blueprint) in parsed)
	  (for max-production = (max-required-production blueprint))
	  (setf *cache* (make-hash-table :test 'equalp))
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
	  (setf *cache* (make-hash-table :test 'equalp))
	  (for geodes = (max-geodes 32
				    (make-mineral)
				    (make-mineral :ore 1)
				    blueprint
				    max-production
				    0))
	  (format t "~a ~a ~a~%"  id geodes (* id geodes))
	  (multiply geodes)))))
