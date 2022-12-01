(asdf:defsystem "advent-of-code-2022"
  :description "Advent of Code 2022"
  :version "0.0.1"
  :author "Blake Watkins <blakewatkins@gmail.com>"
  :licence "GNU General Public License (GPL) version 3"
  :depends-on ("advent-of-code" "iterate" "fset")
  :components ((:file "package")
               (:file "day1" :depends-on ("package"))))
