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
(setf (symbol-function 'mineral>=-int) (map-mineral #'>=))
(setf (symbol-function 'mineral-max) (map-mineral #'max))

(defun mineral>= (a b)
  (let ((ret (mineral>=-int a b)))
    (every (lambda (slot) (slot-value ret slot)) *mineral-types*)))

(defun required-resources (robot-type blueprint)
  (iter
    (for (type resources) in blueprint)
    (finding resources such-that (eq robot-type type))))

;; build a geode robot if possible
;; only build a robot if production of that ore type is less than the maximum
;;   required production of that ore type
(defun available-to-build (resources robots blueprint max-production prevent-types)
  (iter
    (for type in *mineral-types*)
    (for required-resources = (required-resources type blueprint))
    (when (and (mineral>= resources required-resources)
               (or (eq type 'geode)
                   (and (not (member type prevent-types))
                        (< (slot-value robots type)
                           (slot-value max-production type)))))
      (collect type))))

(defun max-required-production (blueprint)
  (iter
    (with ret = (make-mineral))
    (for type in *mineral-types*)
    (for required-resources = (required-resources type blueprint))
    (setf ret (mineral-max ret required-resources))
    (finally (return ret))))

(defun max-geodes (time-remaining resources robots blueprint max-resources prevent-types)
  (break)
  (if (= 0 time-remaining)
      (mineral-geode resources)
      (let ((available-to-build (available-to-build resources
                                                    robots
                                                    blueprint
                                                    max-resources
                                                    prevent-types))
            (new-resources (mineral+ resources robots)))
        (if (member 'geode available-to-build)
            (max-geodes (1- time-remaining)
                        (mineral- new-resources (required-resources 'geode
                                                                    blueprint))
                        (mineral+ robots (make-mineral :geode 1))
                        blueprint
                        max-resources
                        '())                       
            (max
             (or (iter
                   (for type in available-to-build)
                   (for required-resources = (required-resources type
                                                                 blueprint))
                   (maximizing
                    (max-geodes (1- time-remaining)
                                (mineral- new-resources required-resources)
                                (mineral+ robots
                                          (make-mineral
                                           (intern (symbol-name type)
                                                   :keyword)
                                           1))
                                blueprint
                                max-resources
                                '())))
                 0)
             (max-geodes (1- time-remaining)
                         new-resources
                         robots
                         blueprint
                         max-resources
                         available-to-build))))))

(defun day19 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (for (id blueprint) in parsed)
      (for max-production = (max-required-production blueprint))
      (for geodes = (max-geodes 24
                                (make-mineral)
                                (make-mineral :ore 1)
                                blueprint
                                max-production))
      (collect (list id geodes (* id geodes))))))
