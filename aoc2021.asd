(asdf:defsystem "aoc2021"
  :serial t
  :author "Alex Sun <alexsun@mailbox.org>"
  :description "Advent of Code 2021"
  :depends-on (:alexandria
               :uiop
               :iterate
               :split-sequence
               :select
               :str)
  :components ((:file "01/solution")
               (:file "02/solution")
               (:file "03/solution")
               (:file "04/solution")
               (:file "05/solution")
               (:file "06/solution")
               (:file "07/solution")
               (:file "08/solution")
               (:file "09/solution")
               (:file "10/solution")
               (:file "11/solution")
               (:file "12/solution")
               (:file "13/solution")
               (:file "14/solution")
               (:file "15/solution")
               (:file "16/solution")
               (:file "17/solution")
               (:file "18/solution")
               (:file "20/solution")
               (:file "21/solution")))
