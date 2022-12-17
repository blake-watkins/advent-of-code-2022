(asdf:defsystem "advent-of-code-2022"
  :description "Advent of Code 2022"
  :version "0.0.1"
  :author "Blake Watkins <blakewatkins@gmail.com>"
  :licence "GNU General Public License (GPL) version 3"
  :depends-on ("advent-of-code" "iterate" "fset")
  :components ((:file "package")
               (:file "day1" :depends-on ("package"))
               (:file "day2" :depends-on ("package"))
               (:file "day3" :depends-on ("package"))
               (:file "day4" :depends-on ("package"))
               (:file "day5" :depends-on ("package"))
               (:file "day6" :depends-on ("package"))
               (:file "day7" :depends-on ("package"))
               (:file "day8" :depends-on ("package"))
               (:file "day9" :depends-on ("package"))
               (:file "day10" :depends-on ("package"))
               (:file "day11" :depends-on ("package"))
               (:file "day12" :depends-on ("package"))
               (:file "day13" :depends-on ("package"))
               (:file "day14" :depends-on ("package"))
               (:file "day15" :depends-on ("package"))
               (:file "day16" :depends-on ("package"))))
