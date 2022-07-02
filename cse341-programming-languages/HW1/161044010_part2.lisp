; Gebze Technical University
; Computer Engineering Department
; CSE341 Programming Languages
; Homework 0 - Part 2
; Author: Ömer Faruk Bitikçioğlu

(defun set-boundries (input-file)
    (setq input-list (read-boundries input-file))
    (setq start (nth 0 input-list))
    (setq end (nth 1 input-list)))

(defun read-boundries (input-file)
    "Reads boundries from the input file"
    (with-open-file (stream input-file)
        (loop for line = (read stream nil)
            while line
            collect line)))

(defun is-prime (num)
    "Determines if a number is prime or not."
    (loop for i from 2 to (/ num 2) do
        (if (zerop (rem num i)) (return-from is-prime nil)))
    (if (>= num 2) (return-from is-prime t)))

(defun is-semi-prime (num &optional (i 2))
    "Determines if a number is semi-prime or not."
    (cond ((> (expt i 2) num) nil)
        ((zerop (rem num i)) (and (is-prime i) (is-prime (/ num i))))
        (t (is-semi-prime num (+ i 1)))))

(defun test (&optional (input-file "boundries.txt") (output-file "primedistribution.txt"))
    (set-boundries input-file)
    (with-open-file (stream output-file :direction :output)
        (loop for i from start to end do
            (cond ((is-prime i) (print-num-def i "Prime" stream))
                  ((is-semi-prime i) (print-num-def i "Semi-prime" stream))))))

(defun print-num-def (num def out)
    (format out "~D is ~A~%" num def))

(test)
