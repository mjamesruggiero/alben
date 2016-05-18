(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been identified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun setdebug (&rest ids)
  "Start dbg output on the given ids"
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids"
  (setf *dbg-ids* (if (null ids) nil
             (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords"
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun executing-p (x)
  "Is this list in the form of (executing ...)"
  (starts-with x 'executing))

(defun convert-op (op)
  "Make op conform to the (excuting op) convention"
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (executing op) convention"
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))

(defvar *ops* nil "A list of available operators.")

(defstruct op
  "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable"
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;;return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is its add list"
  (member-equal goal (op-add-list op)))

(defun use (oplist)
  "Use oplist as the default list of operators"
  ;; return something useful, but not too verbose
  ;; the number of operators
  (length (setf *ops* oplist)))

;; -------------------------------------------------------------
;; the earlier #achieve and #achieve-all methods Norvig defines
;; -------------------------------------------------------------

;; (defun achieve (state goal goal-stack)
;;   "A goal is achieved if it already holds,
;; or if there is an appropriate op fo it that is applicable"
;;   (dbg-indent :gps (length goal-stack) "Goal ~a" goal)
;;   (cond ((member-equal goal-state) state)
;;         ((member-equal goal goal-stack) nil)
;;         (t (some #'(lambda (op) (apply-op state goal op goal-stack))
;;                  (find-all goal *ops* :test #'appropriate-p)))))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal and make sure they still hold at the end"
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

;; -------------------------------------------------------------
;; the later #achieve and #achieve-all methods Norvig defines
;; -------------------------------------------------------------
(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
or if there is an appropriate opp for it that can be applied"
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas)
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-bananas hungry))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon but uses append instead of nconc"
  (apply #'append (mapcar fn list)))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
           '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
              (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
              (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(defun action-p (x)
  "Is x something that is (start) or (executing)?"
  (or (equal x '(start)) (executing-p x)))

;; -------------------------------------------------------------
;; the newer GPS
;; -------------------------------------------------------------
(defun GPS (state goals &optional (*ops* *ops*))
  "General problem solver: from state, achieve gols using ops"
  (find-all-if #'action-p
               (achieve-all (cons '(start) state) goals nil)))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))"
  (fifth (second action)))

(defun find-path (start end)
  "Search a maze for a path from start to end"
  (let ((results (GPS `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
                          (remove '(start) results
                                  :test #'equal))))))
