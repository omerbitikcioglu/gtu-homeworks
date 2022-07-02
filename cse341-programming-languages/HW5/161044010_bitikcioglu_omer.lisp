; Gebze Technical University
; Computer Engineering Department
; CSE341 Programming Languages
; Homework 5 - Midterm
; Author: Ömer Faruk Bitikçioğlu

(setf facts (list))
(setf predicates (list))
(setf answers (list))

(defun program ()
    (setf horn-clauses 
        (read-file "input.txt"))
    (parse-clauses horn-clauses)
    (print-answers-to-file "output.txt")
)

(defun print-answers-to-file (output-file)
    "Prints answers list to output file"
    (with-open-file (stream output-file :direction :output)
        (format stream "~D" answers)))

(defun read-file (input-file)
    "Reads from given input file"
    (with-open-file (stream input-file)
        (setf l (loop for line = (read stream nil)
            while line
            collect line)))
    (setf l (car l)) ; Remove extra parantheses
    (return-from read-file l))

(defun parse-clauses (horn-clauses)
    "It will answer all the queries in this list of horn-clauses"
    (loop for i from 0 to (- (length horn-clauses) 1) do
        (setf line (nth i horn-clauses))
        (cond 
            ((equal (check-line line) "query") (setf answers (append answers (list (answer-query line)))))
            ((equal (check-line line) "fact") (setf facts (append facts (list line))))
            ((equal (check-line line) "predicates") (setf predicates (append predicates (list line)))))))

(defun check-line (line)
    "Checks line and determines what kind of clause it is"
    (setf head (car line))
    (setf body (cdr line))
    (cond ((null head) (setf typeof "query"))
          ((member nil body) (setf typeof "fact"))
          (t (setf typeof "predicates"))
    (return-from check-line typeof)))

(defun answer-query (query)
    (cond
        ((setf a (check-facts query)) (return-from answer-query a))
        ((setf b (check-predicates query)) (return-from answer-query b))
        (t (return-from answer-query nil))))

(defun check-facts (query)
    (setf found (list))
    (loop for i from 0 to (- (length facts) 1) do
        (if (equal (car(car(cdr query))) (car(car(nth i facts)))) ; Check if it's querying for a fact
            (if (equal (car(car(cdr(car(cdr query))))) (car(car(cdr(car(nth i facts)))))) ; Check if query is correct
                (if ; Compare the integer value of the query to the fact's integer value
                    (eq (car(cdr(car(cdr(car(cdr query)))))) (car(cdr(car(cdr(car(nth i facts)))))))
                    (return-from check-facts t))
                (if ; Check if it is a variable
                    (and 
                        (upper-case-p(char (car(car(cdr(car(cdr query))))) 0))
                        (eq (car(cdr(car(cdr(car(cdr query)))))) (car(cdr(car(cdr(car(nth i facts))))))))
                    (progn 
                        ;(format T "~S = ~S ~%" (car(car(cdr(car(cdr query))))) (car(car(cdr(car(nth i facts))))))
                        (setf found (append found (list(car(car(cdr(car(nth i facts)))))))))))))
    (return-from check-facts found))

(defun check-fact (fact)
    (loop for i from 0 to (- (length facts) 1) do
        (if (equal (car fact) (car(car(nth i facts)))) ; Check if facts are matches
            (if (equal (car(car(cdr fact))) (car(car(cdr(car(nth i facts)))))) ; Check if fact is correct
                (if ; Compare the integer value of the query to the fact's integer value
                    (eq (car(cdr(car(cdr fact)))) (car(cdr(car(cdr(car(nth i facts)))))))
                    (return-from check-fact t))))))

(defun check-predicates (query)
    (if (eq (car query) nil)
        (setf query (car(cdr query))))
    (loop for i from 0 to (- (length predicates) 1) do
        (if (and ; Find the appropriate predicate from predicates
                (equal (car query) (car(car(nth i predicates)))) 
                (equal (car(cdr(car(cdr query)))) (car(cdr(car(cdr(car(nth i predicates)))))))) 
            (if (answer-predicate (nth i predicates) query)
                (return-from check-predicates t)))))

(defun answer-predicate (predicate query-body)
    (setf predicate-body (car(cdr predicate)))
    ;(setf query-body (car(cdr query)))
    (setf name (car(car(cdr query-body))))

    (loop for i from 0 to (- (length predicate-body) 1) do
        (setf (car(car(cdr(nth i predicate-body)))) name)
        (if (and (null (check-fact (nth i predicate-body))) (null (check-predicates (nth i predicate-body))))
            (return-from answer-predicate nil)))

    (return-from answer-predicate t))

(program)