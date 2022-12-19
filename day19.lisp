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

(defparameter *mineral-types* '(geode obsidian clay ore))

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

(defun mineral>= (a b)
  (let ((ret (mineral>=-int a b)))
    (every (lambda (slot) (slot-value ret slot)) *mineral-types*)))

(defun required-resources (robot-type blueprint)
  (iter
    (for (type resources) in blueprint)
    (finding resources such-that (eq robot-type type))))

(defun available-to-build (resources blueprint)
  (iter
    (for type in *mineral-types*)
    (for required-resources = (required-resources type blueprint))
    (when (mineral>= resources required-resources)
      (collect type))))

(defun max-geodes (time-remaining resources robots blueprint)
  (if (= 0 time-remaining)
      (mineral-geode resources)
      (let ((available-to-build (available-to-build resources blueprint))
            (new-resources (mineral+ resources robots)))
        (let ((build-max
                (or
                 (iter
                   (for type in available-to-build)
                   (for required-resources = (required-resources type blueprint))
                   (maximizing
                    (max-geodes (1- time-remaining)
                                (mineral- new-resources required-resources)
                                (mineral+ robots
                                          (make-mineral
                                           (intern (symbol-name type)
                                                   :keyword)
                                           1))
                                blueprint)))
                 0)))
          (max build-max (max-geodes (1- time-remaining)
                                     new-resources
                                     robots
                                     blueprint))))))
