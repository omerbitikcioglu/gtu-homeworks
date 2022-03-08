; Gebze Technical University
; Computer Engineering Department
; CSE341 Programming Languages
; Homework 0 - Part 1
; Author: Ömer Faruk Bitikçioğlu

(defun flattener (input_file)
    "Reads file and flattens it."
    (with-open-file (stream input_file)
        (setq alist 
            (flatten
                (loop for form = (read stream nil stream)
                    until (eq form stream)
                    collect form)))))

(defun flatten (l)
    "Flattens the given list."
    (cond ((null l) l)
        ((atom l) (list l))
        (t (nconc (flatten (car l)) (flatten (cdr l))))))

(defun write-to-file (l output_file)
    "Writes the given list to a file. If the file does not exist, it creates a new file"
    (with-open-file (stream output_file :direction :output)
        (format stream "~{~D ~}~%" l)))

(defun test ()
    "In order to test the program run this. Change file names if you wish."
    (flattener "nested_list.txt")
    (write-to-file alist "flattened_list.txt"))

(test)