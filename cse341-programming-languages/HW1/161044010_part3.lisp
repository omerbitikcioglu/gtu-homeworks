; Gebze Technical University
; Computer Engineering Department
; CSE341 Programming Languages
; Homework 0 - Part 3
; Author: Ömer Faruk Bitikçioğlu

(defun collatz (num out)
    "Recursive function to find collatz sequence of a number"
    (cond ((= num 1) (format out "~D~%" num))
          (t (format out "~D " num)))
    (cond ((zerop (mod num 2)) (collatz (/ num 2) out))
        ((= num 1) (return-from collatz t))
        (t (collatz (+ 1 (* num 3)) out))))

(defun read-nums (input-file)
    "Reads nums from the input file"
    (with-open-file (stream input-file)
        (loop for line = (read stream nil)
            while line
            collect line)))

(defun test ()
    (setq nums (read-nums "integer_inputs.txt"))
    (with-open-file (stream "collatz_outputs.txt" :direction :output)
        (loop for i from 0 to (- (list-length nums) 1) do
            (setq num (nth i nums))
            (format stream "~D: " num)
            (collatz num stream))))

(test)