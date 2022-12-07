(in-package :aoc-2022)

(defun parse-directory-name ()
  (parse-characters #'alphanumericp))

(defun parse-cd ()
  (with-monad
    (parse-string "cd ")
    (either (then (parse-string "..") (unit (list :cd-up)))
            (then (parse-string "/") (unit (list :cd-root)))
            (with-monad
              (assign directory (parse-directory-name))
              (unit (list :cd directory))))))

(defun parse-ls ()
  (then (parse-string "ls") (unit (list :ls))))

(defun parse-command ()
  (with-monad
    (parse-string "$ ")
    (either (parse-cd) (parse-ls))))

(defun parse-directory-entry ()
  (with-monad
    (parse-string "dir ")
    (assign name (parse-directory-name))
    (unit (list :dir name))))

(defun parse-file-name ()
  (parse-characters (lambda (c) (or (alphanumericp c) (char= c #\.)))))

(defun parse-file-entry ()
  (with-monad
    (assign size (parse-number))
    (parse-space)
    (assign name (parse-file-name))
    (unit (list :file name size))))

(defun parse-interaction ()
  (with-monad
    (assign command (parse-line (parse-command)))
    (assign output (zero-or-more (parse-line (either (parse-directory-entry)
                                                     (parse-file-entry)))))
    (unit (list command output))))

(defun parse-file ()
  (one-or-more (parse-interaction)))

(defun get-filesystem (parsed)
  (iter
    (with file-system = (make-hash-table :test 'equal))
    (with current-directory = nil)
    (for (command output) in parsed)
    (for command-name = (first command))
    (case command-name
      (:cd-root (setf current-directory nil))
      (:cd-up (pop current-directory))
      (:cd (push (second command) current-directory))
      (:ls (setf (gethash current-directory file-system) output)))
    (finally (return file-system))))

(defun get-size (directory file-system)
  (let ((contents (gethash directory file-system)))
    (iter
      (for entry in contents)
      (summing (if (eq :file (first entry))
                   (third entry)
                   (get-size (cons (second entry) directory) file-system))))))

(defun day7 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (file-system (get-filesystem parsed)))
    (iter
      (with needed-space = (- 30000000 (- 70000000 (get-size nil file-system))))
      (for (dir entry) in-hashtable file-system)
      (for size = (get-size dir file-system))
      (when (<= size 100000)
        (summing size into part-1))
      (when (> size needed-space)
        (minimizing size into part-2))
      (finally (return (list part-1 part-2))))))

