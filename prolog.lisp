(load "unify.lisp")

;; clauses are represented as (head . body) cons cells
(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))

;; clauses are stored on the predicate's plist
(defun get-clauses (pred)
  (get pred 'clauses))

(defun predicate (relation)
  (first relation))

(defun args (x)
  "The arguments of a relation"
  (rest x))

(defvar *db-predicates*
  nil
  "A list of all predicates stored in the database")

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123"
  (cond ((eq exp '?) (gensym "?"))
        ((atom exp) exp)
        (t (reuse-cons (replace-?-vars (first exp))
                       (replace-?-vars (rest exp))
                       exp))))

(defun add-clause (clause)
  "Add a clause to the database indexed by the head's predicate"
  ;; predicate must be a non-variable symbol
  (let ((pred (predicte (clause-head clause))))))
(defmacro <- (&rest clause)
  "Add a clause to the database"
  `(add-clause ',(replace-?-vars clause)))
