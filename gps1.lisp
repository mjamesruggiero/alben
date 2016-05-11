(defvar *state* nil 
  "Current state: a list of conditions")

(defvar *ops* nil
  "A list of available operators")

(defstruct op
  "An operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defun GPS (*state* goals *ops*)
  "General problem solver: acheive all goals using *ops*"
  (if
    (every #'acheive goals)
    'solved))

(defun acheive (goal)
  "A goal is acheived if it already holds,
  or if there is an appropriate op for it that is acceptable"
  (or (member goal *state*)
      (some #'apply-op
            (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if its in its add list"
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Pring a message and update *state* if op is applicable"
  (when (every #'acheive (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))

(defun find-all (item sequence &rest keyword-args
                      &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
according to the keywords. Doesn't alter sequence."
(if test-not
  (apply #'remove item sequence
         :test-not (complement test-not) keyword-args)
  (apply #'remove item sequence
         :test (complement test) keyword-args)))
